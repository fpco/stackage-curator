{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Monoid
import Data.String (fromString)
import Data.Version
import Options.Applicative
import Filesystem.Path.CurrentOS (decodeString)
import Paths_stackage_curator (version)
import Stackage.CompleteBuild
import Stackage.DiffPlans
import Stackage.Upload
import Stackage.InstallBuild
import Stackage.Stats
import Network.HTTP.Client (withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T

main :: IO ()
main =
    join $
    execParser $
    info
        (helpOption <*> versionOption <*> summaryOption <*> config)
        (header "Stackage" <>
         fullDesc)
  where
    helpOption =
        abortOption ShowHelpText $
        long "help" <>
        help "Show this help text"
    versionOption =
        infoOption
            ("stackage-curator version " ++ showVersion version)
            (long "version" <>
             help "Show stackage version")
    summaryOption =
        infoOption
            "Build package sets for Stackage Nightly and LTS Haskell"
            (long "summary")
    config =
        subparser $
        mconcat
            [ cmnd
                  (uncurry completeBuild)
                  (fmap (Nightly, ) buildFlags)
                  "nightly"
                  "Build, test and upload the Nightly snapshot"
            , cmnd
                  (uncurry completeBuild)
                  (lts Major)
                  "lts-major"
                  "Build, test and upload the LTS (major) snapshot"
            , cmnd
                  (uncurry completeBuild)
                  (lts Minor)
                  "lts-minor"
                  "Build, test and upload the LTS (minor) snapshot"
            , cmnd
                  (const justCheck)
                  (pure ())
                  "check"
                  "Just check that the build plan is ok"
            , cmnd
                  installBuild
                  installFlags
                  "install"
                  "Install a snapshot from an existing build plan"
            , cmnd
                  upload
                  uploadFlags
                  "upload"
                  "Upload a pre-existing bundle"
            , cmnd
                  printStats
                  printStatsFlags
                  "stats"
                  "Print statistics on a build plan"
            , cmnd
                (uncurry diffPlans)
                diffPlansFlags
                "diff"
                "Show the high-level differences between two build plans"
            ]

    cmnd exec parse name desc =
        command name $
        info
            (fmap exec (parse <**> helpOption))
            (progDesc desc)

    buildFlags =
        BuildFlags <$>
        fmap
            not
            (switch
                 (long "skip-tests" <>
                  help "Skip build and running the test suites")) <*>
        fmap
            not
            (switch
                 (long "skip-haddock" <>
                  help "Skip generating haddock documentation")) <*>
        fmap
            not
            (switch
                 (long "skip-upload" <>
                  help "Skip uploading bundle, docs, etc.")) <*>
        switch
            (long "enable-library-profiling" <>
             help "Enable profiling when building") <*>
        switch
            (long "enable-executable-dynamic" <>
             help "Enable dynamic executables when building") <*>
        switch
            (long "verbose" <> short 'v' <>
             help "Output verbose detail about the build steps") <*>
        switch
            (long "skip-check" <>
             help "Skip the check phase, and pass --allow-newer to cabal configure") <*>
        (fmap fromString (strOption
            (long "server-url" <>
             metavar "SERVER-URL" <>
             showDefault <> value (T.unpack $ unStackageServer def) <>
             help "Server to upload bundle to"))) <*>
        fmap
            not
            (switch
                 (long "skip-hoogle" <>
                  help "Skip generating Hoogle input files")) <*>
        (fmap (Just . fromString) (strOption
            (long "bundle-dest" <> metavar "FILENAME"))
            <|> pure Nothing) <*>
        (fmap not (switch
            (long "skip-git-push" <>
             help "Do not perform a git push after completion (for LTS builds only)"))) <*>
        (fmap Just (option
            auto
            (long "jobs" <> short 'j' <>
             metavar "NUMBER" <>
             help "Number of threads")) <|> pure Nothing)

    installFlags =
        InstallFlags <$>
        (fmap
            BPSBundleWeb
            (strOption
                (long "bundle" <>
                 metavar "URL" <>
                 help "Stackage bundle containing build plan")) <|>
         fmap
            (BPSFile . decodeString)
            (strOption
                (long "build-plan" <>
                 metavar "PATH" <>
                 help "Build-plan YAML file"))) <*>
        fmap
            decodeString
            (strArgument
                (metavar "DESTINATION-PATH" <>
                 help "Destination directory path")) <*>
        (fmap
            (Just . decodeString)
            (strOption
                (long "log-dir" <>
                 metavar "PATH" <>
                 help "Location of log files (default DESTINATION-PATH/logs)")) <|>
         pure Nothing) <*>
        option
            auto
            (long "jobs" <>
             metavar "NUMBER" <>
             showDefault <> value 8 <>
             help "Number of threads") <*>
        switch
            (long "global" <>
             help "Install in global package database") <*>
        fmap
            not
            (switch
                (long "skip-tests" <>
                 help "Skip build and running the test suites")) <*>
        fmap
            not
            (switch
                 (long "skip-haddock" <>
                  help "Skip generating haddock documentation")) <*>
        switch
            (long "enable-library-profiling" <>
             help "Enable profiling when building") <*>
        switch
            (long "enable-executable-dynamic" <>
             help "Enable dynamic executables when building") <*>
        switch
            (long "verbose" <> short 'v' <>
             help "Output verbose detail about the build steps") <*>
        switch
            (long "skip-check" <>
             help "Skip the check phase, and pass --allow-newer to cabal configure") <*>
        fmap
            not
            (switch
                 (long "skip-hoogle" <>
                  help "Skip generating Hoogle input files"))

    upload (path, url) = withManager tlsManagerSettings $ \man -> do
        token <- getStackageAuthToken
        res <- flip uploadBundleV2 man UploadBundleV2
            { ub2AuthToken = token
            , ub2Server = fromString url
            , ub2Bundle = decodeString path
            }
        putStrLn $ "New URL: " ++ T.unpack res

    uploadFlags = (,)
        <$> (strArgument
                (metavar "BUNDLE-PATH" <>
                 help "Bundle path"))
        <*> strOption
                (long "server-url" <>
                 metavar "SERVER-URL" <>
                 showDefault <> value (T.unpack $ unStackageServer def) <>
                 help "Server to upload bundle to")

    lts bumpType = (\x y -> (y, x)) -- get the order of arguments correct
        <$> buildFlags
        <*> (LTS bumpType <$> (T.pack <$> argument str
                ( metavar "TARGET-VERSION"
               <> help "Used to run old LTS minor bumps, and rerun broken builds"
               <> value ""
                ) ) )

    printStatsFlags = yamlArg

    diffPlansFlags = (,) <$> yamlArg <*> yamlArg

    yamlArg = fmap decodeString $ strArgument
         $ metavar "YAML-FILE"
        <> help "YAML file containing a build plan"
