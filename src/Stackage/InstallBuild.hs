{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Stackage.InstallBuild
    ( InstallFlags (..)
    , BuildPlanSource (..)
    , installBuild
    ) where

import qualified Codec.Archive.Tar         as Tar
import qualified Codec.Compression.GZip    as GZip
import qualified Data.Yaml                 as Yaml
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Stackage.BuildPlan
import           Stackage.CheckBuildPlan
import           Stackage.PerformBuild
import           Stackage.Prelude
import           System.IO                 (BufferMode (LineBuffering), hSetBuffering)

-- | Flags passed in from the command line.
data InstallFlags = InstallFlags
    { ifPlanSource         :: !BuildPlanSource
    , ifInstallDest        :: !FilePath
    , ifLogDir             :: !(Maybe FilePath)
    , ifJobs               :: !Int
    , ifGlobalInstall      :: !Bool
    , ifEnableTests        :: !Bool
    , ifEnableBenches      :: !Bool
    , ifEnableHaddock      :: !Bool
    , ifEnableLibProfiling :: !Bool
    , ifEnableExecDyn      :: !Bool
    , ifVerbose            :: !Bool
    , ifSkipCheck          :: !Bool
    , ifBuildHoogle        :: !Bool
    , ifNoRebuildCabal     :: !Bool
    } deriving (Show)

-- | Source for build plan.
data BuildPlanSource = BPSBundleWeb String
                     | BPSFile FilePath
    deriving (Show)

getPerformBuild :: BuildPlan -> InstallFlags -> PerformBuild
getPerformBuild plan InstallFlags{..} =
    PerformBuild
    { pbPlan               = plan
    , pbInstallDest        = ifInstallDest
    , pbLogDir             = fromMaybe (ifInstallDest </> "logs") ifLogDir
    , pbLog                = hPut stdout
    , pbJobs               = ifJobs
    , pbGlobalInstall      = ifGlobalInstall
    , pbEnableTests        = ifEnableTests
    , pbEnableBenches      = ifEnableBenches
    , pbEnableHaddock      = ifEnableHaddock
    , pbEnableLibProfiling = ifEnableLibProfiling
    , pbEnableExecDyn      = ifEnableExecDyn
    , pbVerbose            = ifVerbose
    , pbAllowNewer         = ifSkipCheck
    , pbBuildHoogle        = ifBuildHoogle
    , pbNoRebuildCabal     = ifNoRebuildCabal
    , pbCabalFromHead      = False
    }

-- | Install stackage from an existing build plan.
installBuild :: InstallFlags -> IO ()
installBuild installFlags@InstallFlags{..} = do
    hSetBuffering stdout LineBuffering

    putStrLn $ "Loading build plan"
    plan <- case ifPlanSource of
        BPSBundleWeb url -> do
            man <- newManager tlsManagerSettings
            req <- parseUrlThrow url
            res <- httpLbs req man
            planBSL <- getPlanEntry $ Tar.read $ GZip.decompress (responseBody res)
            decodeBuildPlan planBSL
        BPSFile path -> Yaml.decodeFileEither path >>= either throwM return

    if ifSkipCheck
        then putStrLn "Skipping build plan check"
        else do
            putStrLn "Checking build plan"
            checkBuildPlan True plan

    putStrLn "Performing build"
    performBuild (getPerformBuild plan installFlags) >>= mapM_ putStrLn

  where
    getPlanEntry Tar.Done = throwIO NoBuildPlanException
    getPlanEntry (Tar.Fail e) = throwIO e
    getPlanEntry (Tar.Next entry entries)
        | Tar.entryPath entry == "build-plan.yaml" =
            case Tar.entryContent entry of
                Tar.NormalFile bs _ -> return bs
                _ -> throwIO NoBuildPlanException
        | otherwise = getPlanEntry entries

    decodeBuildPlan =
        either throwIO return . Yaml.decodeEither' . toStrict

data InstallBuildException = NoBuildPlanException
    deriving (Typeable)
instance Exception InstallBuildException
instance Show InstallBuildException where
    show NoBuildPlanException = "Bundle has missing or invalid build-plan.yaml"
