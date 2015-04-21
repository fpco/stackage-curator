{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Stackage.CompleteBuild
    ( BuildType (..)
    , BumpType (..)
    , BuildFlags (..)
    , checkPlan
    , getStackageAuthToken
    , createPlan
    , fetch
    ) where

import Control.Concurrent        (threadDelay, getNumCapabilities)
import Control.Concurrent.Async  (withAsync)
import Data.Default.Class        (def)
import Data.Semigroup            (Max (..), Option (..))
import Data.Text.Read            (decimal)
import Data.Time
import Data.Yaml                 (decodeFileEither, encodeFile, decodeEither')
import Network.HTTP.Client
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

nightlySettings :: Text -- ^ day
                -> BuildFlags
                -> BuildPlan
                -> Settings
nightlySettings day bf plan' = Settings
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
    , snapshotType = STNightly
    , bundleDest = fromMaybe
        (fpFromText $ "stackage-nightly-" ++ day ++ ".bundle")
        (bfBundleDest bf)
    }
  where
    slug' = "nightly-" ++ day

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

getSettings :: Manager -> BuildFlags -> BuildType -> Maybe FilePath -> IO Settings
getSettings man bf Nightly mplanFile = do
    day <- tshow . utctDay <$> getCurrentTime
    plan' <- case mplanFile of
        Nothing -> do
            bc <- defaultBuildConstraints man
            pkgs <- getLatestAllowedPlans bc
            newBuildPlan pkgs bc
        Just file -> decodeFileEither (fpToString file) >>= either throwIO return
    return $ nightlySettings day bf plan'
getSettings man bf (LTS bumpType goal) Nothing = do
    matchesGoal <- parseGoal bumpType goal
    Option mlts <- fmap (fmap getMax) $ runResourceT
        $ sourceDirectory "."
       $= concatMapC (parseLTSVer . filename)
       $= filterC matchesGoal
       $$ foldMapC (Option . Just . Max)

    (new, plan') <- case bumpType of
        Major -> do
            let new =
                    case mlts of
                        Nothing -> LTSVer 0 0
                        Just (LTSVer x _) -> LTSVer (x + 1) 0
            bc <- defaultBuildConstraints man
            pkgs <- getLatestAllowedPlans bc
            plan' <- newBuildPlan pkgs bc
            return (new, plan')
        Minor -> do
            old <- maybe (error "No LTS plans found in current directory") return mlts
            oldplan <- decodeFileEither (fpToString $ renderLTSVer old)
                   >>= either throwM return
            let new = incrLTSVer old
            let bc = updateBuildConstraints oldplan
            pkgs <- getLatestAllowedPlans bc
            plan' <- newBuildPlan pkgs bc
            return (new, plan')

    let newfile = renderLTSVer new

    return Settings
        { planFile = fromMaybe newfile (bfPlanFile bf)
        , buildDir = fpFromText $ "builds/lts"
        , logDir = fpFromText $ "logs/stackage-lts-" ++ tshow new
        , title = \ghcVer -> concat
            [ "LTS Haskell "
            , tshow new
            , ", GHC "
            , ghcVer
            ]
        , slug = "lts-" ++ tshow new
        , plan = plan'
        , postBuild = do
            let git args = withCheckedProcess
                    (proc "git" args) $ \ClosedStream Inherited Inherited ->
                        return ()
            putStrLn "Committing new LTS file to Git"
            git ["add", fpToString newfile]
            git ["commit", "-m", "Added new LTS release: " ++ show new]
            when (bfGitPush bf) $ do
                putStrLn "Pushing to Git repository"
                git ["push"]
        , distroName = "LTSHaskell"
        , snapshotType =
            case new of
                LTSVer x y -> STLTS x y
        , bundleDest = fromMaybe
            (fpFromText $ "stackage-lts-" ++ tshow new ++ ".bundle")
            (bfBundleDest bf)
        }

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
           -> IO ()
createPlan target dest = withManager tlsManagerSettings $ \man -> do
    putStrLn $ "Creating plan for: " ++ tshow target
    bc <-
        case target of
            TargetMinor x y -> do
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

    plan <- planFromConstraints bc

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

    ecreds <- tryIO $ readFile "/hackage-creds"
    case map encodeUtf8 $ words $ decodeUtf8 $ either (const "") id ecreds of
        [username, password] -> do
            putStrLn "Uploading as Hackage distro"
            res2 <- uploadHackageDistro distroName plan username password man
            putStrLn $ "Distro upload response: " ++ tshow res2
        _ -> putStrLn "No creds found, skipping Hackage distro upload"
