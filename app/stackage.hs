{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Prelude
import Control.Monad
import Data.String (fromString)
import Data.Version
import Data.Text (pack, stripPrefix)
import Data.Text.Read (decimal)
import Options.Applicative hiding ((<>))
import Filesystem.Path.CurrentOS (decodeString)
import Paths_stackage_curator (version)
import Stackage.CLI
import Stackage.CompleteBuild
import Stackage.DiffPlans
import Stackage.Upload
import Stackage.Update
import Stackage.InstallBuild
import Stackage.Prelude
import Stackage.Stats
import Network.HTTP.Client (withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    join $ fmap snd $ simpleOptions
        $(simpleVersion version)
        "Stackage curation tool"
        "Build package sets for Stackage Nightly and LTS Haskell"
        (pure ())
        commands
  where
    commands = do
        addCommand "update" "Update the package index" id
            (pure $ stackageUpdate defaultStackageUpdateSettings)
        addCommand "create-plan" "Generate a new plan file (possibly based on a previous LTS)" id
            (createPlan <$> target <*> planFile)
        addCommand "check" "Verify that a plan is valid" id
            (checkPlan <$> (fmap Just planFile <|> pure Nothing))
        addCommand "fetch" "Fetch all tarballs needed by a plan" id
            (fetch <$> planFile)
        addCommand "make-bundle" "Run a complete build and generate an upload bundle" id
            makeBundle'
        addCommand "upload" "Upload a bundle to Stackage Server" id
            (upload <$> bundleFile <*> stackageServer)
{-
* `hackage-distro`: Update the Hackage distro list
* `upload-github`: Upload a plan to the relevant Github repo
* `install`: Install a snapshot from an existing build plan
* `stats`: Print statistics on a build plan
* `diff`: Show the high-level differences between two build plans
-}

    makeBundle' = makeBundle
        <$> planFile
        <*> bundleFile
        <*> target
        <*> jobs
        <*> skipTests
        <*> skipHaddock
        <*> skipHoogle
        <*> enableLibraryProfiling
        <*> enableExecutableDynamic
        <*> verbose
        <*> allowNewer


    {-
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
                 (long "skip-tests" ++
                  help "Skip build and running the test suites")) <*>
        fmap
            not
            (switch
                 (long "skip-haddock" ++
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
        (fmap (Just . fromString) (strOption
            (long "plan-file" <> metavar "FILENAME"))
            <|> pure Nothing) <*>
        (switch
            (long "pre-build" <>
             help "Only perform operations up until the actual build")) <*>
        (switch
            (long "load-plan" <>
             help "Load plan from file"))

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
    -}

    jobs =
        (fmap Just (option
            auto
            (long "jobs" ++ short 'j' ++
             metavar "NUMBER" ++
             help "Number of threads")) <|> pure Nothing)

    skipTests =
        switch
            (long "skip-tests" ++
             help "Skip build and running the test suites")

    skipHaddock =
        switch
            (long "skip-haddock" ++
             help "Skip generating haddock documentation")

    skipHoogle =
        switch
            (long "skip-hoogle" ++
             help "Skip generating Hoogle input files")

    enableLibraryProfiling =
        switch
            (long "enable-library-profiling" ++
             help "Enable profiling when building")

    enableExecutableDynamic =
        switch
            (long "enable-executable-dynamic" ++
             help "Enable dynamic executables when building")

    verbose =
        switch
            (long "verbose" ++ short 'v' ++
             help "Output verbose detail about the build steps")

    allowNewer =
        switch
            (long "allow-newer" ++
             help "Pass --allow-newer to cabal configure (useful when skipping checks)")

    target :: Parser Target
    target = option readTarget
        ( metavar "TARGET"
       ++ long "target"
       ++ help "Build target: nightly, lts-X.Y"
        )

    readTarget :: ReadM Target
    readTarget = do
        s <- str
        let onErr = fail $ "Invalid target: " ++ s
        case s of
            "nightly" -> return TargetNightly
            'l':'t':'s':'-':t1 -> maybe onErr return $ do
                Right (i, t2) <- Just $ decimal $ T.pack t1
                if T.null t2
                    then return $ TargetMajor i
                    else do
                        t3 <- T.stripPrefix (T.pack ".") t2
                        Right (j, t4) <- Just $ decimal t3
                        guard $ T.null t4
                        if j == 0
                            then return $ TargetMajor i
                            else return $ TargetMinor i j
            _ -> onErr

    planFile = fmap decodeString $ strOption
         ( metavar "YAML-FILE"
        ++ long "plan-file"
        ++ help "YAML file containing a build plan"
         )

    bundleFile = fmap decodeString $ strOption
         ( metavar "BUNDLE-FILE"
        ++ long "bundle-file"
        ++ help "Path to bundle file"
         )

    stackageServer =
        (fmap fromString (strOption
            (long "server-url" ++
             metavar "SERVER-URL" ++
             showDefault ++ value (T.unpack $ unStackageServer def) ++
             help "Server to upload bundle to")))
