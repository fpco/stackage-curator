{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
module Stackage.CompleteBuild
    ( BuildFlags (..)
    , checkPlan
    , getStackageAuthToken
    , createPlan
    , fetch
    , makeBundle
    , upload
    , hackageDistro
    , uploadGithub
    , uploadDocs'
    , checkTargetAvailable
    ) where

import System.Directory (getAppUserDataDirectory)
import Distribution.Package (Dependency)
import Filesystem (createTree, isFile, rename)
import Filesystem.Path (parent)
import qualified Filesystem.Path.CurrentOS as F
import Control.Concurrent        (threadDelay, getNumCapabilities)
import Control.Concurrent.Async  (withAsync)
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
import Filesystem.Path           (dropExtension)
import Control.Monad.Trans.Unlift (askRunBase, MonadBaseUnlift)
import Data.Function (fix)
import Control.Concurrent.Async (Concurrently (..))
import Stackage.Curator.UploadDocs (uploadDocs)
import System.Directory (doesDirectoryExist, doesFileExist)

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

createPlan :: Target
           -> FilePath
           -> [Dependency] -- ^ additional constraints
           -> [PackageName] -- ^ newly added packages
           -> [PackageName] -- ^ newly expected test failures
           -> [PackageName] -- ^ newly expected haddock failures
           -> IO ()
createPlan target dest constraints addPackages expectTestFailures expectHaddockFailures = do
    man <- newManager tlsManagerSettings
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

    plan <- planFromConstraints
          $ flip (foldr expectHaddockFailure) expectHaddockFailures
          $ flip (foldr expectTestFailure) expectTestFailures
          $ flip (foldr addPackage) addPackages
          $ setConstraints constraints bc

    putStrLn $ "Writing build plan to " ++ pack dest
    encodeFile dest plan
  where
    -- Add a new package to the build constraints
    addPackage :: PackageName -> BuildConstraints -> BuildConstraints
    addPackage name bc = bc { bcPackages = insertSet name $ bcPackages bc }

    expectTestFailure = tweak $ \pc -> pc { pcTests = ExpectFailure }
    expectHaddockFailure = tweak $ \pc -> pc { pcHaddocks = ExpectFailure }

    tweak f name bc = bc
        { bcPackageConstraints = \name' ->
            (if name == name' then f else id)
            (bcPackageConstraints bc name')
        }

planFromConstraints :: MonadIO m => BuildConstraints -> m BuildPlan
planFromConstraints bc = do
    putStrLn "Creating build plan"
    plans <- getLatestAllowedPlans bc
    newBuildPlan plans bc

-- | Just print a message saying "still alive" every minute, to appease Travis.
stillAlive :: IO () -> IO ()
stillAlive inner =
    withAsync (printer 1) $ const inner
  where
    printer i = forever $ do
        threadDelay 60000000
        putStrLn $ "Still alive: " ++ tshow i
        printer $! i + (1 :: Int)

-- | Generate and check a new build plan, but do not execute it.
--
-- Since 0.3.1
checkPlan :: Maybe FilePath -> IO ()
checkPlan mfp = stillAlive $ do
    man <- newManager tlsManagerSettings
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
                putStrLn $ "Loading plan from " ++ pack fp
                decodeFileEither fp >>= either throwM return

    putStrLn "Checking plan"
    checkBuildPlan plan

    putStrLn "Plan seems valid!"

{- FIXME remove
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

-- | Make a complete plan, build, test and upload bundle, docs and
-- distro.
completeBuild :: BuildType -> BuildFlags -> IO ()
completeBuild buildType buildFlags = do
    man <- newManager tlsManagerSettings
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
            encodeFile planFile plan

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
hackageDistro planFile target = do
    man <- newManager tlsManagerSettings
    plan <- decodeFileEither planFile >>= either throwM return
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

checkoutRepo :: Target -> IO ([String] -> IO (), FilePath, FilePath)
checkoutRepo target = do
    root <- fmap (</> "curator") $ getAppUserDataDirectory "stackage"

    let repoDir =
            case target of
                TargetNightly _ -> root </> "stackage-nightly"
                TargetLts _ _ -> root </> "lts-haskell"

        runIn wdir cmd args = do
            putStrLn $ concat
                [ pack wdir
                , ": "
                , tshow (cmd:args)
                ]
            withCheckedProcess
                (proc cmd args)
                    { cwd = Just wdir
                    } $ \ClosedStream Inherited Inherited -> return ()

        git = runIn repoDir "git"

        name =
            case target of
                TargetNightly day -> concat
                    [ "nightly-"
                    , show day
                    , ".yaml"
                    ]
                TargetLts x y -> concat
                    [ "lts-"
                    , show x
                    , "."
                    , show y
                    , ".yaml"
                    ]

        destFPPlan = repoDir </> name
        destFPDocmap = repoDir </> "docs" </> name

    exists <- doesDirectoryExist repoDir
    if exists
        then do
            git ["fetch"]
            git ["checkout", "origin/master"]
        else do
            createTree $ parent $ fromString repoDir
            runIn "." "git" ["clone", repoUrl, repoDir]

    whenM (liftIO $ doesFileExist destFPPlan)
        $ error $ "File already exists: " ++ destFPPlan
    whenM (liftIO $ doesFileExist destFPDocmap)
        $ error $ "File already exists: " ++ destFPDocmap

    return (git, destFPPlan, destFPDocmap)
  where
    repoUrl =
        case target of
            TargetNightly _ -> "git@github.com:fpco/stackage-nightly"
            TargetLts _ _ -> "git@github.com:fpco/lts-haskell"

uploadGithub
    :: FilePath -- ^ plan file
    -> FilePath -- ^ docmap file
    -> Target
    -> IO ()
uploadGithub planFile docmapFile target = do
    (git, destFPPlan, destFPDocmap) <- checkoutRepo target

    createTree $ parent $ fromString destFPDocmap
    runResourceT $ do
        sourceFile planFile $$ (sinkFile destFPPlan :: Sink ByteString (ResourceT IO) ())
        sourceFile docmapFile $$ (sinkFile destFPDocmap :: Sink ByteString (ResourceT IO) ())

    git ["add", destFPPlan, destFPDocmap]
    git ["commit", "-m", "Checking in " ++ F.encodeString (dropExtension $ fromString destFPPlan)]
    git ["push", "origin", "HEAD:master"]

upload
    :: FilePath -- ^ bundle file
    -> StackageServer -- ^ server URL
    -> IO ()
upload bundleFile server = do
    man <- newManager tlsManagerSettings
    putStrLn "Uploading bundle to Stackage Server"

    token <- getStackageAuthToken

    res <- flip uploadBundleV2 man UploadBundleV2
        { ub2Server = server
        , ub2AuthToken = token
        , ub2Bundle = bundleFile
        }
    putStrLn $ "New snapshot available at: " ++ res

uploadDocs' :: Target
            -> FilePath -- ^ bundle file
            -> IO ()
uploadDocs' target bundleFile = do
    name <-
        case target of
            TargetNightly day -> return $ "nightly-" ++ tshow day
            TargetLts x y -> return $ concat ["lts-", tshow x, ".", tshow y]
    uploadDocs
        (installDest target </> "doc")
        bundleFile
        name
        "haddock.stackage.org"

installDest :: Target -> FilePath
installDest target =
    case target of
        TargetNightly _ -> "builds/nightly"
        TargetLts x _ -> unpack $ "builds/lts-" ++ tshow x

makeBundle
    :: FilePath -- ^ plan file
    -> FilePath -- ^ docmap file
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
  planFile docmapFile bundleFile target mjobs skipTests skipHaddocks skipHoogle
  enableLibraryProfiling enableExecutableDynamic verbose allowNewer
        = do
    plan <- decodeFileEither planFile >>= either throwM return
    jobs <- maybe getNumCapabilities return mjobs
    let pb = PerformBuild
            { pbPlan = plan
            , pbInstallDest = installDest target
            , pbLog = hPut stdout
            , pbLogDir =
                case target of
                    TargetNightly _ -> "logs/nightly"
                    TargetLts x _ -> unpack $ "logs/lts-" ++ tshow x
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

    putStrLn $ "Creating bundle (v2) at: " ++ pack bundleFile
    createBundleV2 CreateBundleV2
        { cb2Plan = plan
        , cb2Type =
            case target of
                TargetNightly day -> STNightly2 day
                TargetLts x y -> STLTS x y
        , cb2DocsDir = pbDocDir pb
        , cb2Dest = bundleFile
        , cb2DocmapFile = docmapFile
        }

fetch :: FilePath -> IO ()
fetch planFile = do
    man <- newManager tlsManagerSettings
    -- First make sure to fetch all of the dependencies... just in case Hackage
    -- has an outage. Don't feel like wasting hours of CPU time.
    putStrLn "Pre-fetching all packages"

    plan <- decodeFileEither planFile >>= either throwM return

    cabalDir <- getAppUserDataDirectory "cabal"
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
                let tmp = F.encodeString fp <.> "tmp"
                runResourceT $ bodyReaderSource (responseBody res) $$ sinkFile tmp
                rename (fromString tmp) fp
      where
        url = unpack $ concat
            [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
            , name
            , "-"
            , version
            , ".tar.gz"
            ]
        fp = sdistFilePath cabalDir name version

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

-- | Check if the given target is already used in the Github repos
checkTargetAvailable :: Target -> IO ()
checkTargetAvailable = void . checkoutRepo
