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
-- | Dealing with the 00-index file and all its cabal files.
module Stackage.PackageIndex
    ( sourcePackageIndex
    , UnparsedCabalFile (..)
    , SimplifiedPackageDescription (..)
    , SimplifiedComponentInfo (..)
    , getLatestDescriptions
    ) where

import qualified Codec.Archive.Tar                     as Tar
import           Data.Conduit.Lazy                     (MonadActive,
                                                        lazyConsume)
import qualified Data.Text                             as T
import           Distribution.Compiler                 (CompilerFlavor)
import           Distribution.ModuleName               (ModuleName)
import           Distribution.Package                  (Dependency)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Distribution.ParseUtils               (PError)
import           Distribution.System                   (Arch, OS)
import           Stackage.Prelude
import           Stackage.Update
import           Stackage.GithubPings
import           System.Directory                      (doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing)
import           System.FilePath                       (takeDirectory)
import qualified Data.Binary                           as Bin (Binary)
import qualified Data.Binary.Tagged                    as Bin
import qualified Data.ByteString.Base16                as B16
import qualified Crypto.Hash.SHA256                    as SHA256
import           Language.Haskell.Extension            (Extension, Language, KnownExtension)
import           Data.Proxy

-- | Name of the 00-index.tar downloaded from Hackage.
getPackageIndexPath :: MonadIO m => m FilePath
getPackageIndexPath = liftIO $ do
    c <- getCabalRoot
    let configFile = c </> "config"
    exists <- liftIO $ doesFileExist configFile
    remoteCache <- if exists
        then do
            configLines <- runResourceT $ sourceFile (c </> "config")
                                       $$ decodeUtf8C
                                       =$ linesUnboundedC
                                       =$ concatMapC getRemoteCache
                                       =$ sinkList
            case configLines of
                [x] -> return x
                [] -> error $ "No remote-repo-cache found in Cabal config file"
                _ -> error $ "Multiple remote-repo-cache entries found in Cabal config file"
        else return $ c </> "packages"

    let tarball = remoteCache </> "hackage.haskell.org" </> "00-index.tar"

    unlessM (liftIO $ doesFileExist tarball) $
        stackageUpdate defaultStackageUpdateSettings

    return tarball
  where
    getCabalRoot :: IO FilePath
    getCabalRoot = getAppUserDataDirectory "cabal"

    getRemoteCache s = do
        ("remote-repo-cache", stripPrefix ":" -> Just v) <- Just $ break (== ':') s
        Just $ unpack $ T.strip v

-- | A cabal file with name and version parsed from the filepath, and the
-- package description itself ready to be parsed. It's left in unparsed form
-- for efficiency.
data UnparsedCabalFile = UnparsedCabalFile
    { ucfName    :: PackageName
    , ucfVersion :: Version
    , ucfPath    :: FilePath
    , ucfContent :: LByteString
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
    , spdCondLibrary :: Maybe (CondTree ConfVar [Dependency] SimplifiedComponentInfo)
    , spdCondExecutables :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdCondTestSuites :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdCondBenchmarks :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdPackageFlags :: Map FlagName Bool
    , spdGithubPings :: Set Text
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
    structuralInfo x = Bin.NominalType
        "CondTree ConfVar [Dependency]"
        -- FIXME? (Bin.structuralInfo $ getInnerProxy x)
      where
        getInnerProxy :: Proxy (CondTree c v a) -> Proxy a
        getInnerProxy _ = Proxy

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

gpdToSpd :: GenericPackageDescription -> SimplifiedPackageDescription
gpdToSpd gpd = SimplifiedPackageDescription
    { spdName = name
    , spdVersion = version
    , spdCondLibrary = fmap (mapCondTree simpleLib) $ condLibrary gpd
    , spdCondExecutables = map (fmap $ mapCondTree simpleExe) $ condExecutables gpd
    , spdCondTestSuites = map (fmap $ mapCondTree simpleTest) $ condTestSuites gpd
    , spdCondBenchmarks = map (fmap $ mapCondTree simpleBench) $ condBenchmarks gpd
    , spdPackageFlags =
        let getFlag MkFlag {..} = (flagName, flagDefault)
         in mapFromList $ map getFlag $ genPackageFlags gpd
    , spdGithubPings = getGithubPings gpd
    }
  where
    PackageIdentifier name version = package $ packageDescription gpd

    simpleLib = helper getModules libBuildInfo
    simpleExe = helper noModules buildInfo
    simpleTest = helper noModules testBuildInfo
    simpleBench = helper noModules benchmarkBuildInfo

    helper getModules getBI x = SimplifiedComponentInfo
        { sciBuildTools = buildTools $ getBI x
        , sciModules = getModules x
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
ucfParse root (UnparsedCabalFile name version fp lbs) = liftIO $ do
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
    parseFromText =
        case parsePackageDescription $ unpack $ dropBOM $ decodeUtf8 lbs of
            ParseFailed e -> throwM $ CabalParseException fp e
            ParseOk _warnings gpd -> do
                let pd = packageDescription gpd
                    PackageIdentifier name' version' = package pd
                when (name /= name' || version /= version') $
                    throwM $ MismatchedNameVersion fp
                        name name' version version'
                return $ gpdToSpd gpd

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
