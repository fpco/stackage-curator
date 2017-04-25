{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
-- | Dealing with the 00-index file and all its cabal files.
module Stackage.PackageIndex
    ( sourcePackageIndex
    , UnparsedCabalFile (..)
    , SimplifiedPackageDescription (..)
    , SimplifiedComponentInfo (..)
    , getLatestDescriptions
    , gpdFromLBS
    , getAllCabalHashesCommit
    ) where

import qualified Codec.Archive.Tar                     as Tar
import           Data.Conduit.Lazy                     (MonadActive,
                                                        lazyConsume)
import qualified Data.Text                             as T
import           Distribution.Compiler                 (CompilerFlavor)
import           Distribution.Version                  (VersionRange (..))
import           Distribution.Package                  (Dependency)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Distribution.ParseUtils               (PError)
import           Distribution.System                   (Arch, OS)
import           Stackage.Prelude
import           Stackage.GithubPings
import           System.Directory                      (getAppUserDataDirectory, createDirectoryIfMissing, doesFileExist)
import           System.FilePath                       (takeDirectory)
import qualified Data.ByteString.Base16                as B16
import qualified Crypto.Hash.SHA256                    as SHA256
import           Crypto.Hash                 (MD5 (..), SHA1 (..), SHA256 (..),
                                              SHA512 (..), Skein512_512 (..), hashlazy,
                                              Digest, HashAlgorithm, digestToHexByteString)
import qualified Crypto.Hash.SHA1 as SHA1
import           Data.Store                            (Store)
import qualified Data.Store                            as Store
import qualified Data.Store.TypeHash                   as Store

-- | Name of the 00-index.tar downloaded from Hackage.
getPackageIndexPath :: MonadIO m => m FilePath
getPackageIndexPath = liftIO $ do
    stackRoot <- getAppUserDataDirectory "stack"
    let tarballs =
            [ stackRoot </> "indices" </> "Hackage" </> "01-index.tar"
            , stackRoot </> "indices" </> "Hackage" </> "00-index.tar"
            ]
        loop [] = error $ "tarballs not found: " ++ show tarballs
        loop (x:xs) = do
            exists <- doesFileExist x
            if exists
                then return x
                else loop xs
    loop tarballs

-- | Get the Git commit of the all-cabal-hashes repo at its current state
getAllCabalHashesCommit :: MonadIO m => m (Either SomeException Text)
getAllCabalHashesCommit = liftIO $ do
    stackRoot <- getAppUserDataDirectory "stack"
    let dir = stackRoot </> "indices" </> "Hackage" </> "git-update" </> "all-cabal-hashes"
        cp = (proc "git" ["rev-list", "-n", "1", "current-hackage"]) { cwd = Just dir }
    tryAny $ withCheckedProcessCleanup cp $ \ClosedStream out ClosedStream ->
        out $$ takeWhileCE (/= 10) =$ decodeUtf8C =$ foldC

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
instance Store SimplifiedComponentInfo

data SimplifiedPackageDescription = SimplifiedPackageDescription
    { spdName :: PackageName
    , spdVersion :: Version
    , spdCabalFileInfo :: CabalFileInfo
    , spdCondLibrary :: Maybe (CondTree ConfVar [Dependency] SimplifiedComponentInfo)
    , spdCondExecutables :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdCondTestSuites :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdCondBenchmarks :: [(String, CondTree ConfVar [Dependency] SimplifiedComponentInfo)]
    , spdSetupDeps :: Maybe [Dependency]
    , spdPackageFlags :: Map FlagName Bool
    , spdGithubPings :: Set Text
    , spdCabalVersion :: Maybe Version
    }
    deriving Generic

#if !MIN_VERSION_base(4, 9, 0)
deriving instance Generic Version
#endif

instance Store SimplifiedPackageDescription
instance Store a => Store (CondTree ConfVar [Dependency] a)
instance Store Dependency
instance Store v => Store (Condition v)
instance Store ConfVar
instance Store Arch
instance Store OS
instance Store CompilerFlavor
instance Store PackageName
instance Store Version
instance Store VersionRange
instance Store FlagName
-- END orphans

Store.mkManyHasTypeHash
    [ [t|SimplifiedPackageDescription|]
    ]

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
    , spdSetupDeps = fmap setupDepends $ setupBuildInfo $ packageDescription gpd
    , spdPackageFlags =
        let getFlag MkFlag {..} = (flagName, flagDefault)
         in mapFromList $ map getFlag $ genPackageFlags gpd
    , spdGithubPings = getGithubPings gpd
    , spdCabalVersion =
        case specVersionRaw $ packageDescription gpd of
          Left v -> Just v
          Right range0 ->
              let maxRange AnyVersion = Nothing
                  maxRange (ThisVersion v) = Just v
                  maxRange (LaterVersion v) = Just v
                  -- ideally the following case would never happen
                  maxRange (EarlierVersion v) = Just v
                  maxRange (WildcardVersion v) = Just v
                  maxRange (UnionVersionRanges x y) = maxRanges x y
                  maxRange (IntersectVersionRanges x y) = maxRanges x y
                  maxRange (VersionRangeParens x) = maxRange x

                  maxRanges x y =
                      case (maxRange x, maxRange y) of
                          (Nothing, Nothing) -> Nothing
                          (Just x', Nothing) -> Just x'
                          (Nothing, Just y') -> Just y'
                          (Just x', Just y') -> Just (max x' y')
               in maxRange range0
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
    eres <- tryIO' $ fmap Store.decode $ readFile cache
    case eres of
        Right (Right (Store.Tagged x)) -> return x
        _ -> do
            x <- parseFromText
            createDirectoryIfMissing True $ takeDirectory cache
            writeFile cache $ Store.encode $ Store.Tagged x
            return x
  where
    tryIO' :: IO a -> IO (Either IOException a)
    tryIO' = try

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
                      -> (SimplifiedPackageDescription -> Either SomeException desc)
                      -> m (Map PackageName desc, Map PackageName Version)
getLatestDescriptions f parseDesc = liftIO $ do
    root <- fmap (</> "curator") $ getAppUserDataDirectory "stackage"

    -- Parse twice to avoid keeping stuff in memory: once to determine which
    -- versions to keep, once to do the actual parsing.
    liftIO $ putStrLn "Determining target package versions"
    (mvers, latests) <- runResourceT $ sourcePackageIndex $$ getZipSink ((,)
        -- get the latest, given the filter
        <$> ZipSink (filterC f' =$ flip foldlC mempty
            (\m ucf -> insertWith max (ucfName ucf) (ucfVersion ucf) m))
        -- get the absolute latest
        <*> ZipSink (flip foldlC mempty
            (\m ucf -> insertWith max (ucfName ucf) (ucfVersion ucf) m)))
    liftIO $ putStrLn "Parsing package descriptions"
    plans <- runResourceT $ sourcePackageIndex $$ flip foldMC mempty
        (\m ucf ->
            if lookup (ucfName ucf) (asMap mvers) == Just (ucfVersion ucf)
                then do
                    edesc <- liftIO $ parseDesc <$> ucfParse root ucf
                    case edesc of
                        Left e -> print e $> m
                        Right desc -> return $! insertMap (ucfName ucf) desc m
                else return m)
    return (plans, latests)
  where
    f' ucf = f (ucfName ucf) (ucfVersion ucf)
