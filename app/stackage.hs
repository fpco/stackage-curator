{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, TupleSections #-}

module Main where

import           Control.Monad
import qualified Data.Text                    as T
import           Data.Text.Read               (decimal)
import           Options.Applicative
import           Paths_stackage_curator       (version)
import qualified Prelude
import           Stackage.CLI
import           Stackage.CompleteBuild
import           Stackage.Curator.UploadIndex
import           Stackage.DiffPlans
import           Stackage.InstallBuild
import           Stackage.Prelude             hiding ((<>))
import           Stackage.Stats
import           Stackage.Update
import           Stackage.Upload
import           System.IO                    (BufferMode (LineBuffering), hSetBuffering)

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
            (createPlan
                <$> target
                <*> planFile
                <*> many constraint
                )
        addCommand "check" "Verify that a plan is valid" id
            (checkPlan <$> (fmap Just planFile <|> pure Nothing))
        addCommand "fetch" "Fetch all tarballs needed by a plan" id
            (fetch <$> planFile)
        addCommand "make-bundle" "Run a complete build and generate an upload bundle" id
            makeBundle'
        addCommand "check-target-available" "Is the given target available to be used?" id
            (checkTargetAvailable <$> target)
        addCommand "upload" "Upload a bundle to Stackage Server" id
            (upload <$> bundleFile <*> stackageServer)
        addCommand "hackage-distro" "Update the Hackage distro list" id
            (hackageDistro <$> planFile <*> target)
        addCommand "upload-github" "Upload a plan to the relevant Github repo" id
            (uploadGithub <$> planFile <*> docmapFile <*> target)
        addCommand "install" "Install a snapshot from an existing build plan" id
            (installBuild <$> installFlags)
        addCommand "stats" "Print statistics on a build plan" id
            (printStats <$> planFile)
        addCommand "diff" "Show the high-level differences between two build plans" id
            (diffPlans <$> planFileArg <*> planFileArg
                       <*> diffsOnly <*> useColor <*> githubFetch <*> html)
        addCommand "upload-index" "Upload the 00-index.tar.gz file to S3" id
            (uploadIndex
                <$> planFile
                <*> target
                <*> pure (T.pack "haddock.stackage.org")
                <*> pure (T.pack "package-index/"))
        addCommand "upload-docs" "Upload documentation to an S3 bucket" id
            (uploadDocs' <$> target <*> bundleFile)

    makeBundle' = makeBundle
        <$> planFile
        <*> docmapFile
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


    installFlags :: Parser InstallFlags
    installFlags =
        InstallFlags <$>
        (fmap
            BPSBundleWeb
            (strOption
                (long "bundle" <>
                 metavar "URL" <>
                 help "Stackage bundle containing build plan")) <|>
         fmap
            BPSFile
            (strOption
                (long "build-plan" <>
                 metavar "PATH" <>
                 help "Build-plan YAML file"))) <*>
         (strArgument
                (metavar "DESTINATION-PATH" <>
                 help "Destination directory path")) <*>
        (fmap
            Just
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
            'n':'i':'g':'h':'t':'l':'y':'-':t ->
                maybe onErr (return . TargetNightly) (readMay t)
            'l':'t':'s':'-':t1 -> maybe onErr return $ do
                Right (i, t2) <- Just $ decimal $ T.pack t1
                if T.null t2
                    then return $ TargetLts i 0
                    else do
                        t3 <- T.stripPrefix (T.pack ".") t2
                        Right (j, t4) <- Just $ decimal t3
                        guard $ T.null t4
                        return $ TargetLts i j
            _ -> onErr

    planFile = strOption
         ( metavar "YAML-FILE"
        ++ long "plan-file"
        ++ help "YAML file containing a build plan"
         )

    docmapFile = strOption
         ( metavar "YAML-FILE"
        ++ long "docmap-file"
        ++ help "YAML file containing the docmap (list of all generated Haddock modules)"
         )

    planFileArg = strArgument
         ( metavar "YAML-FILE"
        ++ help "YAML file containing a build plan"
         )

    bundleFile = strOption
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

    constraint =
        option constraintRead
            (long "constraint" ++
             metavar "CONSTRAINT" ++
             help "New constraints for plan construction")

    constraintRead = do
        s <- str
        case simpleParse $ T.pack s of
            Nothing -> fail $ "Invalid constraint: " ++ s
            Just d -> return d

    diffsOnly =
        switch
            (long "diffsOnly" <> short 'd' <>
             help "Show changed packages only")

    useColor =
        switch
            (long "useColor" <> short 'c' <>
             help "Show differences in color")

    githubFetch =
        switch
            (long "githubFetch" <> short 'g' <>
             help "Fetch YAML files from GitHub")

    html =
        switch
            (long "html" <> short 'h' <>
             help "Wrap the output in HTML <ul>/<li> tags")
