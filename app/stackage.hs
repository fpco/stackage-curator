{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, TupleSections #-}

module Main where

import           Control.Monad
import qualified Data.Text                    as T
import           Data.Text.Read               (decimal)
import           Options.Applicative
import           Options.Applicative.Simple   (simpleOptions, simpleVersion, addCommand)
import           Paths_stackage_curator       (version)
import           Stackage.CompleteBuild
import           Stackage.Curator.RevDeps
import           Stackage.Curator.UploadIndex
import           Stackage.DiffPlans
import           Stackage.InstallBuild
import           Stackage.Prelude             hiding ((<>))
import           Stackage.Stats
import           Data.Monoid                  ((<>))

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
        addCommand "create-plan" "Generate a new plan file (possibly based on a previous LTS)" id
            (createPlan
                <$> target
                <*> planFile
                <*> many constraint
                <*> many addPackage
                <*> many expectTestFailure
                <*> many expectBenchFailure
                <*> many expectHaddockFailure
                )
        addCommand "check" "Verify that a plan is valid" id
            (checkPlan <$> (fmap Just planFile <|> pure Nothing))
        addCommand "fetch" "Fetch all tarballs needed by a plan" id
            (fetch <$> planFile)
        addCommand "make-bundle" "Run a complete build and generate an upload bundle" id
            makeBundle'
        addCommand "check-target-available" "Is the given target available to be used?" id
            (checkTargetAvailable <$> target)
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
            (uploadDocs' <$> target)
        addCommand "list-revdeps" "List reverse dependencies" id
            (listRevDeps <$> planFile <*> deepRevDeps <*> revDepPackage)

    makeBundle' = makeBundle
        <$> planFile
        <*> docmapFile
        <*> target
        <*> jobs
        <*> skipTests
        <*> skipBenches
        <*> skipHaddock
        <*> skipHoogle
        <*> enableLibraryProfiling
        <*> enableExecutableDynamic
        <*> verbose
        <*> allowNewer
        <*> noRebuildCabal
        <*> cabalFromHead


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
                (long "skip-benches" <>
                 help "Skip building the benchmarks")) <*>
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
                  help "Skip generating Hoogle input files")) <*>
        noRebuildCabal

    noRebuildCabal =
        switch
            (long "no-rebuild-cabal" <>
             help "Ignore new Cabal version from the plan and use whatever's in the database. Useful for testing pre-release GHCs")

    cabalFromHead =
        switch
            (long "cabal-from-head" <>
             help "Get the latest Cabal from Git HEAD, used for testing Cabal itself")

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

    skipBenches =
        switch
            (long "skip-benches" ++
             help "Skip building the benchmarks")

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

    addPackage =
        option packageRead
            (long "add-package" ++
             metavar "PACKAGE-NAME" ++
             help "Newly added package")

    expectTestFailure =
        option packageRead
            (long "expect-test-failure" ++
             metavar "PACKAGE-NAME" ++
             help "Newly expected test failures")

    expectBenchFailure =
        option packageRead
            (long "expect-bench-failure" ++
             metavar "PACKAGE-NAME" ++
             help "Newly expected benchmark build failures")

    expectHaddockFailure =
        option packageRead
            (long "expect-haddock-failure" ++
             metavar "PACKAGE-NAME" ++
             help "Newly expected haddock failures")

    packageRead = do
        s <- str
        case simpleParse $ T.pack s of
            Nothing -> fail $ "Invalid package name: " ++ s
            Just p -> return p

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

    deepRevDeps =
        switch
            (long "deep" <>
             help "List deep reverse dependencies, not just immediate users")

    revDepPackage = argument packageRead
        (metavar "PACKAGE-NAME" ++
         help "Package to list reverse deps for")
