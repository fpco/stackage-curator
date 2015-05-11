{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
module Stackage.CompleteBuild
    ( BuildType (..)
    , BumpType (..)
    , BuildFlags (..)
    , checkPlan
    , getStackageAuthToken
    , createPlan
    , fetch
    , makeBundle
    , upload
    , hackageDistro
    , uploadGithub
    , uploadDocs'
    ) where

import System.Directory (getAppUserDataDirectory)
import Distribution.Package (Dependency)
import Filesystem (isDirectory, createTree, isFile, rename)
import Filesystem.Path (parent)
import Control.Concurrent        (threadDelay, getNumCapabilities)
import Control.Concurrent.Async  (withAsync)
import Data.Default.Class        (def)
import Data.Semigroup            (Max (..), Option (..))
import Data.Text.Read            (decimal)
import Data.Time
import Data.Yaml                 (decodeFileEither, encodeFile, decodeEither')
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Stackage.BuildConstraints
import Stackage.BuildPlan
import Stackage.CheckBuildPlan
import Stackage.PerformBuild
import Stackage.Prelude
import Stackage.ServerBundle
import Stackage.UpdateBuildPlan
import Stackage.Upload
import System.Environment        (lookupEnv)
import System.IO                 (BufferMode (LineBuffering), hSetBuffering)
import Control.Monad.Trans.Unlift (askRunBase, MonadBaseUnlift)
import Data.Function (fix)
import Control.Concurrent.Async (Concurrently (..))
import Stackage.Curator.UploadDocs (uploadDocs)

-- | Flags passed in from the command line.
data BuildFlags = BuildFlags
    { bfEnableTests      :: !Bool
    , bfEnableHaddock    :: !Bool
    , bfDoUpload         :: !Bool
    , bfEnableLibProfile :: !Bool
    , bfEnableExecDyn    :: !Bool
    , bfVerbose          :: !Bool
    , bfSkipCheck        :: !Bool
    , bfServer           :: !StackageServer
    , bfBuildHoogle      :: !Bool
    , bfBundleDest       :: !(Maybe FilePath)
    , bfGitPush          :: !Bool
    -- ^ push to Git (when doing an LTS build)
    , bfJobs             :: !(Maybe Int)
    , bfPlanFile         :: !(Maybe FilePath)
    , bfPreBuild         :: !Bool
    , bfLoadPlan         :: !Bool
    } deriving (Show)

data BuildType = Nightly | LTS BumpType Text
    deriving (Show, Read, Eq, Ord)

data BumpType = Major | Minor
    deriving (Show, Read, Eq, Ord)

data Settings = Settings
    { plan      :: BuildPlan
    , planFile  :: FilePath
    , buildDir  :: FilePath
    , logDir    :: FilePath
    , title     :: Text -> Text -- ^ GHC version -> title
    , slug      :: Text
    , postBuild :: IO ()
    , distroName :: Text -- ^ distro name on Hackage
    , snapshotType :: SnapshotType
    , bundleDest :: FilePath
    }

nightlyPlanFile :: Text -- ^ day
                -> FilePath
nightlyPlanFile day = fpFromText ("nightly-" ++ day) <.> "yaml"

nightlySettings :: Day
                -> BuildFlags
                -> BuildPlan
                -> Settings
nightlySettings day' bf plan' = Settings
    { planFile = fromMaybe (nightlyPlanFile day) (bfPlanFile bf)
    , buildDir = fpFromText $ "builds/nightly"
    , logDir = fpFromText $ "logs/stackage-nightly-" ++ day
    , title = \ghcVer -> concat
        [ "Stackage Nightly "
        , day
        , ", GHC "
        , ghcVer
        ]
    , slug = slug'
    , plan = plan'
    , postBuild = return ()
    , distroName = "Stackage"
    , snapshotType = STNightly2 day'
    , bundleDest = fromMaybe
        (fpFromText $ "stackage-nightly-" ++ day ++ ".bundle")
        (bfBundleDest bf)
    }
  where
    slug' = "nightly-" ++ day
    day = tshow day'

parseGoal :: MonadThrow m
          => BumpType
          -> Text
          -> m (LTSVer -> Bool)
parseGoal _ "" = return $ const True
parseGoal bumpType t =
    case decimal t of
        Right (major, "") -> return $ \(LTSVer major' _) ->
            case bumpType of
                -- For major bumps: specifying 2 means we want to ignore
                -- anything in the 2.* range
                Major -> major' < major

                -- But for minor bumps, specifying 2 means we want to include
                -- everything in 2.*, and start ignore 3.*
                Minor -> major' <= major
        _ ->
            case parseLTSRaw t of
                Nothing -> throwM $ ParseGoalFailure t
                Just x -> return (< x)

data ParseGoalFailure = ParseGoalFailure Text
    deriving (Show, Typeable)
instance Exception ParseGoalFailure

data LTSVer = LTSVer !Int !Int
    deriving (Eq, Ord)
instance Show LTSVer where
    show (LTSVer x y) = concat [show x, ".", show y]
incrLTSVer :: LTSVer -> LTSVer
incrLTSVer (LTSVer x y) = LTSVer x (y + 1)

parseLTSVer :: FilePath -> Maybe LTSVer
parseLTSVer fp = do
    w <- stripPrefix "lts-" $ fpToText fp
    x <- stripSuffix ".yaml" w
    parseLTSRaw x

parseLTSRaw :: Text -> Maybe LTSVer
parseLTSRaw x = do
    Right (major, y) <- Just $ decimal x
    z <- stripPrefix "." y
    Right (minor, "") <- Just $ decimal z
    return $ LTSVer major minor

createPlan :: Target
           -> FilePath
           -> [Dependency] -- ^ additional constraints
           -> IO ()
createPlan target dest constraints = withManager tlsManagerSettings $ \man -> do
    putStrLn $ "Creating plan for: " ++ tshow target
    bc <-
        case target of
            TargetLts x y | y /= 0 -> do
                let url = concat
                        [ "https://raw.githubusercontent.com/fpco/lts-haskell/master/lts-"
                        , show x
                        , "."
                        , show (y - 1)
                        , ".yaml"
                        ]
                putStrLn $ "Downloading old plan from " ++ pack url
                req <- parseUrl url
                res <- httpLbs req man
                oldplan <- either throwM return
                         $ decodeEither' (toStrict $ responseBody res)
                return $ updateBuildConstraints oldplan
            _ -> defaultBuildConstraints man

    plan <- planFromConstraints $ setConstraints constraints bc

    putStrLn $ "Writing build plan to " ++ fpToText dest
    encodeFile (fpToString dest) plan

planFromConstraints bc = do
    putStrLn "Creating build plan"
    plans <- getLatestAllowedPlans bc
    newBuildPlan plans bc

renderLTSVer :: LTSVer -> FilePath
renderLTSVer lts = fpFromText $ concat
    [ "lts-"
    , tshow lts
    , ".yaml"
    ]

-- | Just print a message saying "still alive" every minute, to appease Travis.
stillAlive :: IO () -> IO ()
stillAlive inner =
    withAsync (printer 1) $ const inner
  where
    printer i = forever $ do
        threadDelay 60000000
        putStrLn $ "Still alive: " ++ tshow i
        printer $! i + 1

-- | Generate and check a new build plan, but do not execute it.
--
-- Since 0.3.1
checkPlan :: Maybe FilePath -> IO ()
checkPlan mfp = stillAlive $ withManager tlsManagerSettings $ \man -> do
    plan <-
        case mfp of
            Nothing -> do
                putStrLn "Loading default build constraints"
                bc <- defaultBuildConstraints man

                plan <- planFromConstraints bc

                putStrLn $ "Writing build plan to check-plan.yaml"
                encodeFile "check-plan.yaml" plan

                return plan
            Just fp -> do
                putStrLn $ "Loading plan from " ++ fpToText fp
                decodeFileEither (fpToString fp) >>= either throwM return

    putStrLn "Checking plan"
    checkBuildPlan plan

    putStrLn "Plan seems valid!"

getPerformBuild :: BuildFlags -> Settings -> IO PerformBuild
getPerformBuild buildFlags Settings {..} = do
    jobs <- maybe getNumCapabilities return $ bfJobs buildFlags
    return PerformBuild
        { pbPlan = plan
        , pbInstallDest = buildDir
        , pbLogDir = logDir
        , pbLog = hPut stdout
        , pbJobs = jobs
        , pbGlobalInstall = False
        , pbEnableTests = bfEnableTests buildFlags
        , pbEnableHaddock = bfEnableHaddock buildFlags
        , pbEnableLibProfiling = bfEnableLibProfile buildFlags
        , pbEnableExecDyn = bfEnableExecDyn buildFlags
        , pbVerbose = bfVerbose buildFlags
        , pbAllowNewer = bfSkipCheck buildFlags
        , pbBuildHoogle = bfBuildHoogle buildFlags
        }

{- FIXME remove
-- | Make a complete plan, build, test and upload bundle, docs and
-- distro.
completeBuild :: BuildType -> BuildFlags -> IO ()
completeBuild buildType buildFlags = withManager tlsManagerSettings $ \man -> do
    hSetBuffering stdout LineBuffering

    settings@Settings {..} <- if bfLoadPlan buildFlags
        then
            case bfPlanFile buildFlags of
                Nothing -> error "When loading plan, plan file must be specified"
                Just file -> do
                    putStrLn $ "Loading build plan from: " ++ fpToText file
                    getSettings man buildFlags buildType $ Just file
        else do
            putStrLn $ "Loading settings for: " ++ tshow buildType
            settings@Settings {..} <- getSettings man buildFlags buildType Nothing

            putStrLn $ "Writing build plan to: " ++ fpToText planFile
            encodeFile (fpToString planFile) plan

            if bfSkipCheck buildFlags
                then putStrLn "Skipping build plan check"
                else do
                    putStrLn "Checking build plan"
                    checkBuildPlan plan

            return settings

    pb <- getPerformBuild buildFlags settings

    if bfPreBuild buildFlags
        then prefetchPackages pb
        else do
            putStrLn "Performing build"
            performBuild pb >>= mapM_ putStrLn

            putStrLn $ "Creating bundle (v2) at: " ++ fpToText bundleDest
            createBundleV2 CreateBundleV2
                { cb2Plan = plan
                , cb2Type = snapshotType
                , cb2DocsDir = pbDocDir pb
                , cb2Dest = bundleDest
                }

            postBuild `catchAny` print

            when (bfDoUpload buildFlags) $
                finallyUpload
                    buildFlags
                    settings
                    man
-}

getStackageAuthToken :: IO Text
getStackageAuthToken = do
    mtoken <- lookupEnv "STACKAGE_AUTH_TOKEN"
    case mtoken of
        Nothing -> decodeUtf8 <$> readFile "/auth-token"
        Just token -> return $ pack token

{- FIXME remove
-- | The final part of the complete build process: uploading a bundle,
-- docs and a distro to hackage.
finallyUpload :: BuildFlags
              -> Settings -> Manager -> IO ()
finallyUpload buildFlags settings@Settings{..} man = do
    let server = bfServer buildFlags
    pb <- getPerformBuild buildFlags settings

    putStrLn "Uploading bundle to Stackage Server"

    token <- getStackageAuthToken

    res <- flip uploadBundleV2 man UploadBundleV2
        { ub2Server = server
        , ub2AuthToken = token
        , ub2Bundle = bundleDest
        }
    putStrLn $ "New snapshot available at: " ++ res

-}

hackageDistro
    :: FilePath -- ^ plan file
    -> Target
    -> IO ()
hackageDistro planFile target = withManager tlsManagerSettings $ \man -> do
    plan <- decodeFileEither (fpToString planFile) >>= either throwM return
    ecreds <- tryIO $ readFile "/hackage-creds"
    case map encodeUtf8 $ words $ decodeUtf8 $ either (const "") id ecreds of
        [username, password] -> do
            putStrLn $ "Uploading as Hackage distro: " ++ distroName
            res2 <- uploadHackageDistro distroName plan username password man
            putStrLn $ "Distro upload response: " ++ tshow res2
        _ -> error "No Hackage creds found at /hackage-creds"
  where
    distroName =
        case target of
            TargetNightly _ -> "Stackage"
            TargetLts _ _ -> "LTSHaskell"

uploadGithub
    :: FilePath -- ^ plan file
    -> Target
    -> IO ()
uploadGithub planFile target = do
    let repoUrl =
            case target of
                TargetNightly _ -> "git@github.com:fpco/stackage-nightly"
                TargetLts _ _ -> "git@github.com:fpco/lts-haskell"

    root <- fmap (</> "curator") $ fpFromString <$> getAppUserDataDirectory "stackage"

    now <- getCurrentTime

    let repoDir =
            case target of
                TargetNightly _ -> root </> "stackage-nightly"
                TargetLts _ _ -> root </> "lts-haskell"

        destFP =
            case target of
                TargetNightly day -> repoDir </> (fpFromString $ concat
                    [ "nightly-"
                    , show day
                    , ".yaml"
                    ])
                TargetLts x y -> repoDir </> (fpFromString $ concat
                    [ "lts-"
                    , show x
                    , "."
                    , show y
                    , ".yaml"
                    ])

        runIn wdir cmd args = do
            putStrLn $ concat
                [ fpToText wdir
                , ": "
                , tshow (cmd:args)
                ]
            withCheckedProcess
                (proc cmd args)
                    { cwd = Just $ fpToString wdir
                    } $ \ClosedStream Inherited Inherited -> return ()

        git = runIn repoDir "git"

    exists <- isDirectory repoDir
    if exists
        then do
            git ["fetch"]
            git ["checkout", "origin/master"]
        else do
            createTree $ parent repoDir
            runIn "." "git" ["clone", repoUrl, fpToString repoDir]

    runResourceT $ sourceFile planFile $$ (sinkFile destFP :: Sink ByteString (ResourceT IO) ())
    git ["add", fpToString destFP]
    git ["commit", "-m", "Checking in " ++ fpToString (filename destFP)]
    git ["push", "origin", "HEAD:master"]

upload
    :: FilePath -- ^ bundle file
    -> StackageServer -- ^ server URL
    -> IO ()
upload bundleFile server = withManager tlsManagerSettings $ \man -> do
    putStrLn "Uploading bundle to Stackage Server"

    token <- getStackageAuthToken

    res <- flip uploadBundleV2 man UploadBundleV2
        { ub2Server = server
        , ub2AuthToken = token
        , ub2Bundle = bundleFile
        }
    putStrLn $ "New snapshot available at: " ++ res

uploadDocs' :: Target -> IO ()
uploadDocs' target = do
    name <-
        case target of
            TargetNightly day -> return $ "nightly-" ++ tshow day
            TargetLts x y -> return $ concat ["lts-", tshow x, ".", tshow y]
    uploadDocs
        (installDest target </> "doc")
        name
        "haddock.stackage.org"

installDest :: Target -> FilePath
installDest target =
    case target of
        TargetNightly _ -> "builds/nightly"
        TargetLts x _ -> fpFromText $ "builds/lts-" ++ tshow x

makeBundle
    :: FilePath -- ^ plan file
    -> FilePath -- ^ bundle file
    -> Target
    -> Maybe Int -- ^ jobs
    -> Bool -- ^ skip tests?
    -> Bool -- ^ skip haddock?
    -> Bool -- ^ skip hoogle?
    -> Bool -- ^ enable library profiling?
    -> Bool -- ^ enable executable dynamic?
    -> Bool -- ^ verbose?
    -> Bool -- ^ allow-newer?
    -> IO ()
makeBundle
  planFile bundleFile target mjobs skipTests skipHaddocks skipHoogle
  enableLibraryProfiling enableExecutableDynamic verbose allowNewer
        = do
    plan <- decodeFileEither (fpToString planFile) >>= either throwM return
    jobs <- maybe getNumCapabilities return mjobs
    let pb = PerformBuild
            { pbPlan = plan
            , pbInstallDest = installDest target
            , pbLog = hPut stdout
            , pbLogDir =
                case target of
                    TargetNightly _ -> "logs/nightly"
                    TargetLts x _ -> fpFromText $ "logs/lts-" ++ tshow x
            , pbJobs = jobs
            , pbGlobalInstall = False
            , pbEnableTests = not skipTests
            , pbEnableHaddock = not skipHaddocks
            , pbEnableLibProfiling = enableLibraryProfiling
            , pbEnableExecDyn = enableExecutableDynamic
            , pbVerbose = verbose
            , pbAllowNewer = allowNewer
            , pbBuildHoogle = not skipHoogle
            }

    putStrLn "Performing build"
    performBuild pb >>= mapM_ putStrLn

    putStrLn $ "Creating bundle (v2) at: " ++ fpToText bundleFile
    createBundleV2 CreateBundleV2
        { cb2Plan = plan
        , cb2Type =
            case target of
                TargetNightly day -> STNightly2 day
                TargetLts x y -> STLTS x y
        , cb2DocsDir = pbDocDir pb
        , cb2Dest = bundleFile
        }

fetch :: FilePath -> IO ()
fetch planFile = withManager tlsManagerSettings $ \man -> do
    -- First make sure to fetch all of the dependencies... just in case Hackage
    -- has an outage. Don't feel like wasting hours of CPU time.
    putStrLn "Pre-fetching all packages"

    plan <- decodeFileEither (fpToString planFile) >>= either throwM return

    cabalDir <- fpFromString <$> getAppUserDataDirectory "cabal"
    parMapM_ 8 (download man cabalDir) $ mapToList $ bpPackages plan
  where
    download man cabalDir (display -> name, display . ppVersion -> version) = do
        unlessM (isFile fp) $ do
            hPut stdout $ encodeUtf8 $ concat
                [ "Downloading "
                , name
                , "-"
                , version
                , "\n"
                ]
            createTree $ parent fp
            req <- parseUrl url
            withResponse req man $ \res -> do
                let tmp = fp <.> "tmp"
                runResourceT $ bodyReaderSource (responseBody res) $$ sinkFile tmp
                rename tmp fp
      where
        url = unpack $ concat
            [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
            , name
            , "-"
            , version
            , ".tar.gz"
            ]
        fp = cabalDir </>
             "packages" </>
             "hackage.haskell.org" </>
             fpFromText name </>
             fpFromText version </>
             fpFromText (concat [name, "-", version, ".tar.gz"])

parMapM_ :: (MonadIO m, MonadBaseUnlift IO m, MonoFoldable mono)
         => Int
         -> (Element mono -> m ())
         -> mono
         -> m ()
parMapM_ (max 1 -> 1) f xs = mapM_ f xs
parMapM_ cnt f xs0 = do
    var <- liftBase $ newTVarIO $ toList xs0
    run <- askRunBase
    let worker :: IO ()
        worker = run $ fix $ \loop -> join $ atomically $ do
            xs <- readTVar var
            case xs of
                [] -> return $ return ()
                x:xs' -> do
                    writeTVar var xs'
                    return $ do
                        f x
                        loop
        workers 1 = Concurrently worker
        workers i = Concurrently worker *> workers (i - 1)
    liftBase $ runConcurrently $ workers cnt
