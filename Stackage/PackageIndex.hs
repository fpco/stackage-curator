{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Dealing with the 00-index file and all its cabal files.
module Stackage.PackageIndex
    ( sourcePackageIndex
    , UnparsedCabalFile (..)
    , SimplifiedPackageDescription (..)
    , SimplifiedComponentInfo (..)
    , getLatestDescriptions
    , gpdFromLBS
    ) where

import qualified Codec.Archive.Tar                     as Tar
import           Data.Conduit.Lazy                     (MonadActive,
                                                        lazyConsume)
import qualified Data.Text                             as T
import           Distribution.Compiler                 (CompilerFlavor)
import           Distribution.Package                  (Dependency)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Distribution.ParseUtils               (PError)
import           Distribution.System                   (Arch, OS)
import           Stackage.Prelude
import           Stackage.GithubPings
import           System.Directory                      (getAppUserDataDirectory, createDirectoryIfMissing)
import           System.FilePath                       (takeDirectory)
import qualified Data.Binary                           as Bin (Binary)
import           Data.Binary.Orphans                   ()
import qualified Data.Binary.Tagged                    as Bin
import qualified Data.ByteString.Base16                as B16
import qualified Crypto.Hash.SHA256                    as SHA256
import           Data.Proxy
import           Crypto.Hash                 (MD5 (..), SHA1 (..), SHA256 (..),
                                              SHA512 (..), Skein512_512 (..), hashlazy,
                                              Digest, HashAlgorithm, digestToHexByteString)
import qualified Crypto.Hash.SHA1 as SHA1

-- | Name of the 00-index.tar downloaded from Hackage.
getPackageIndexPath :: MonadIO m => m FilePath
getPackageIndexPath = liftIO $ do
    stackRoot <- getAppUserDataDirectory "stack"
    let tarball = stackRoot </> "indices" </> "Hackage" </> "00-index.tar"
    return tarball

-- | A cabal file with name and version parsed from the filepath, and the
-- package description itself ready to be parsed. It's left in unparsed form
-- for efficiency.
data UnparsedCabalFile = UnparsedCabalFile
    { ucfName    :: PackageName
    , ucfVersion :: Version
    , ucfPath    :: FilePath
    , ucfContent :: LByteString
    , ucfEntry   :: Tar.Entry
    }

data SimplifiedComponentInfo = SimplifiedComponentInfo
    { sciBuildTools :: [Dependency]
    , sciModules :: Set Text
    }
    deriving Generic
instance Bin.Binary SimplifiedComponentInfo
instance Bin.HasStructuralInfo SimplifiedComponentInfo
instance Bin.HasSemanticVersion SimplifiedComponentInfo

data SimplifiedPackageDescription = SimplifiedPackageDescription
    { spdName :: PackageName
    , spdVersion :: Version
    , spdCabalFileInfo :: CabalFileInfo
    , spdCondLibrary :: Maybe (CondTree ConfVar [Dependency] SimplifiedComponentInfo)
    , spdCondExecutables :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdCondTestSuites :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdCondBenchmarks :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdPackageFlags :: Map FlagName Bool
    , spdGithubPings :: Set Text
    , spdCabalVersion :: Either Version VersionRange
    }
    deriving Generic
instance Bin.Binary SimplifiedPackageDescription
instance Bin.HasStructuralInfo SimplifiedPackageDescription
instance Bin.HasSemanticVersion SimplifiedPackageDescription

-- BEGIN orphans
deriving instance Generic (CondTree v c a)
deriving instance Generic (Condition c)
deriving instance Generic ConfVar

instance (Bin.Binary v, Bin.Binary c, Bin.Binary a) => Bin.Binary (CondTree v c a)
instance Bin.Binary c => Bin.Binary (Condition c)
instance Bin.Binary ConfVar

-- special treatment for recursive datatype
instance Bin.HasStructuralInfo a => Bin.HasStructuralInfo (CondTree ConfVar [Dependency] a) where
    structuralInfo _x = Bin.NominalType
        "CondTree ConfVar [Dependency]"
        -- FIXME? (Bin.structuralInfo $ getInnerProxy x)

instance Bin.HasStructuralInfo Dependency
instance Bin.HasStructuralInfo v => Bin.HasStructuralInfo (Condition v) where
    structuralInfo x = Bin.NominalNewtype
        "Condition"
        (Bin.structuralInfo $ getInnerProxy x)
      where
        getInnerProxy :: Proxy (Condition v) -> Proxy v
        getInnerProxy _ = Proxy
instance Bin.HasStructuralInfo ConfVar
instance Bin.HasStructuralInfo Arch
instance Bin.HasStructuralInfo OS
instance Bin.HasStructuralInfo CompilerFlavor
instance Bin.HasStructuralInfo PackageName
instance Bin.HasStructuralInfo VersionRange
instance Bin.HasStructuralInfo FlagName
-- END orphans

gpdToSpd :: LByteString -- ^ raw cabal file contents
         -> GenericPackageDescription -> SimplifiedPackageDescription
gpdToSpd raw gpd = SimplifiedPackageDescription
    { spdName = name
    , spdVersion = version
    , spdCabalFileInfo = CabalFileInfo
        { cfiSize = length raw
        , cfiHashes =
            let go :: (Show ha, HashAlgorithm ha) => ha -> (Text, Text)
                go constr = (tshow constr, unwrap constr (hashlazy raw))
                unwrap :: ha -> Digest ha -> Text
                unwrap _ = decodeUtf8 . digestToHexByteString
             in mapFromList
                    [ go SHA1
                    , go SHA256
                    , go SHA512
                    , go Skein512_512
                    , go MD5
                    , ("GitSHA1", decodeUtf8 $ B16.encode $ SHA1.hashlazy $ concat
                        [ "blob "
                        , fromStrict $ encodeUtf8 $ tshow $ length raw
                        , "\0"
                        , raw
                        ])
                    ]
        }
    , spdCondLibrary = mapCondTree simpleLib <$> condLibrary gpd
    , spdCondExecutables = map (fmap $ mapCondTree simpleExe) $ condExecutables gpd
    , spdCondTestSuites = map (fmap $ mapCondTree simpleTest) $ condTestSuites gpd
    , spdCondBenchmarks = map (fmap $ mapCondTree simpleBench) $ condBenchmarks gpd
    , spdPackageFlags =
        let getFlag MkFlag {..} = (flagName, flagDefault)
         in mapFromList $ map getFlag $ genPackageFlags gpd
    , spdGithubPings = getGithubPings gpd
    , spdCabalVersion = specVersionRaw $ packageDescription gpd
    }
  where
    PackageIdentifier name version = package $ packageDescription gpd

    simpleLib = helper getModules libBuildInfo
    simpleExe = helper noModules buildInfo
    simpleTest = helper noModules testBuildInfo
    simpleBench = helper noModules benchmarkBuildInfo

    helper getModules' getBI x = SimplifiedComponentInfo
        { sciBuildTools = buildTools $ getBI x
        , sciModules = getModules' x
        }

    noModules = const mempty
    getModules = setFromList . map display . exposedModules

deriving instance Functor (CondTree v c)

mapCondTree :: (a -> b) -> CondTree v c a -> CondTree v c b
mapCondTree = fmap

ucfParse :: MonadIO m
         => FilePath -- ^ ~/.stackage/curator
         -> UnparsedCabalFile
         -> m SimplifiedPackageDescription
ucfParse root (UnparsedCabalFile name version fp lbs _entry) = liftIO $ do
    eres <- tryIO $ Bin.taggedDecodeFileOrFail cache
    case eres of
        Right (Right x) -> return x
        _ -> do
            x <- parseFromText
            createDirectoryIfMissing True $ takeDirectory cache
            Bin.taggedEncodeFile cache x
            return x
  where
    -- location of the binary cache
    cache = root </> "cache" </> (unpack $ decodeUtf8 $ B16.encode $ SHA256.hashlazy lbs)

    -- Parse the desc from the contents of the .cabal file
    parseFromText = do
        gpd <- gpdFromLBS fp lbs
        let pd = packageDescription gpd
            PackageIdentifier name' version' = package pd
        when (name /= name' || version /= version') $
            throwM $ MismatchedNameVersion fp
                name name' version version'
        return $ gpdToSpd lbs gpd

gpdFromLBS :: MonadThrow m
           => FilePath
           -> LByteString
           -> m GenericPackageDescription
gpdFromLBS fp lbs =
    case parsePackageDescription $ unpack $ dropBOM $ decodeUtf8 lbs of
        ParseFailed e -> throwM $ CabalParseException fp e
        ParseOk _warnings gpd -> return gpd
  where
    -- https://github.com/haskell/hackage-server/issues/351
    dropBOM t = fromMaybe t $ stripPrefix "\xFEFF" t

-- | Stream all of the cabal files from the 00-index tar file.
sourcePackageIndex :: (MonadThrow m, MonadResource m, MonadActive m, MonadBaseControl IO m)
                   => Producer m UnparsedCabalFile
sourcePackageIndex = do
    fp <- getPackageIndexPath
    -- yay for the tar package. Use lazyConsume instead of readFile to get some
    -- kind of resource protection
    lbs <- lift $ fromChunks <$> lazyConsume (sourceFile fp)
    loop (Tar.read lbs)
  where
    loop (Tar.Next e es) = goE e >> loop es
    loop Tar.Done = return ()
    loop (Tar.Fail e) = throwM e

    goE e
        | Just front <- stripSuffix ".cabal" $ pack $ Tar.entryPath e
        , Tar.NormalFile lbs _size <- Tar.entryContent e = do
            (name, version) <- parseNameVersion front
            yield UnparsedCabalFile
                { ucfName = name
                , ucfVersion = version
                , ucfPath = Tar.entryPath e
                , ucfContent = lbs
                , ucfEntry = e
                }
        | otherwise = return ()

    parseNameVersion t1 = do
        let (p', t2) = break (== '/') $ T.replace "\\" "/" t1
        p <- simpleParse p'
        t3 <- maybe (throwM $ InvalidCabalPath t1 "no slash") return
            $ stripPrefix "/" t2
        let (v', t4) = break (== '/') t3
        v <- simpleParse v'
        when (t4 /= cons '/' p') $ throwM $ InvalidCabalPath t1 $ "Expected at end: " ++ p'
        return (p, v)

data InvalidCabalPath = InvalidCabalPath Text Text
    deriving (Show, Typeable)
instance Exception InvalidCabalPath

data CabalParseException = CabalParseException FilePath PError
                         | MismatchedNameVersion FilePath PackageName PackageName Version Version
    deriving (Show, Typeable)
instance Exception CabalParseException

-- | Get all of the latest descriptions for name/version pairs matching the
-- given criterion.
getLatestDescriptions :: MonadIO m
                      => (PackageName -> Version -> Bool)
                      -> (SimplifiedPackageDescription -> IO desc)
                      -> m (Map PackageName desc)
getLatestDescriptions f parseDesc = liftIO $ do
    root <- fmap (</> "curator") $ getAppUserDataDirectory "stackage"

    -- Parse twice to avoid keeping stuff in memory: once to determine which
    -- versions to keep, once to do the actual parsing.
    liftIO $ putStrLn "Determining target package versions"
    mvers <- runResourceT $ sourcePackageIndex $$ filterC f' =$ flip foldlC mempty
        (\m ucf -> insertWith max (ucfName ucf) (ucfVersion ucf) m)
    liftIO $ putStrLn "Parsing package descriptions"
    runResourceT $ sourcePackageIndex $$ flip foldMC mempty
        (\m ucf ->
            if lookup (ucfName ucf) (asMap mvers) == Just (ucfVersion ucf)
                then do
                    desc <- liftIO $ ucfParse root ucf >>= parseDesc
                    return $! insertMap (ucfName ucf) desc m
                else return m)
  where
    f' ucf = f (ucfName ucf) (ucfVersion ucf)
