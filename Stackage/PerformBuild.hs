-- | Perform an actual build, generate a binary package database and a
-- documentation directory in the process.
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stackage.PerformBuild
    ( performBuild
    , PerformBuild (..)
    , BuildException (..)
    , pbDocDir
    , sdistFilePath
    ) where

import           Control.Concurrent.Async    (forConcurrently_)
import           Control.Monad.Writer.Strict (execWriter, tell)
import qualified Data.ByteString             as S
import           Data.Generics               (mkT, everywhere)
import qualified Data.Map                    as Map
import           Data.NonNull                (fromNullable)
import           Distribution.PackageDescription (buildType, packageDescription, BuildType (Simple),
                                                 condTestSuites, condBenchmarks)
import           Distribution.Package        (Dependency (..))
import           Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import           Distribution.Types.UnqualComponentName
import           Distribution.Version        (anyVersion)
import           Filesystem                  (canonicalizePath, createTree,
                                              getWorkingDirectory,
                                              removeTree, rename, removeFile)
import           Filesystem.Path             (parent)
import qualified Filesystem.Path.CurrentOS   as F
import           Network.HTTP.Simple
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.GhcPkg
import           Stackage.PackageDescription
import           Stackage.PackageIndex       (gpdFromLBS)
import           Stackage.Prelude            hiding (pi)
import           System.Directory            (doesDirectoryExist, doesFileExist, findExecutable, getDirectoryContents)
import qualified System.Directory
import qualified System.FilePath             as FP
import           System.Environment          (getEnvironment)
import           System.Exit
import           System.IO                   (IOMode (WriteMode),
                                              openBinaryFile, hFlush)
import           System.IO.Temp              (withSystemTempDirectory, withSystemTempFile)
import           System.Timeout              (timeout)

data BuildException = BuildException (Map PackageName BuildFailure) [Text]
    deriving Typeable
instance Exception BuildException
instance Show BuildException where
    show (BuildException m warnings) =
        unlines $ "" : "" : "" : map go (mapToList m) ++ map unpack warnings
      where
        go (unPackageName -> name, bf) = concat
            [ name
            , ": "
            , take 500 $ show bf
            ]

data BuildFailure = DependencyFailed PackageName
                  | DependencyMissing PackageName
                  | ToolMissing ExeName
                  | NotImplemented
                  | BuildFailureException SomeException
    deriving (Show, Typeable)
instance Exception BuildFailure

data PerformBuild = PerformBuild
    { pbPlan          :: BuildPlan
    , pbInstallDest   :: FilePath
    , pbLog           :: ByteString -> IO ()
    , pbLogDir        :: FilePath
    , pbJobs          :: Int
    , pbGlobalInstall :: Bool
    -- ^ Register packages in the global database
    , pbEnableTests        :: Bool
    , pbEnableBenches      :: Bool
    , pbEnableHaddock      :: Bool
    , pbEnableLibProfiling :: Bool
    , pbEnableExecDyn      :: Bool
    , pbVerbose            :: Bool
    , pbAllowNewer         :: Bool
    -- ^ Strip out version bounds in .cabal files
    , pbBuildHoogle        :: Bool
    -- ^ Should we build Hoogle database?
    --
    -- May be disabled due to: https://ghc.haskell.org/trac/ghc/ticket/9921
    , pbNoRebuildCabal     :: !Bool
    -- ^ Ignore new Cabal version from the plan and use whatever's in the
    -- database. Useful for testing pre-release GHCs
    , pbCabalFromHead      :: !Bool
    -- ^ Used for testing Cabal itself: grab the most recent version of Cabal
    -- from Github master
    }

data PackageInfo = PackageInfo
    { piPlan   :: PackagePlan
    , piName   :: PackageName
    , piResult :: TMVar Bool
    }

waitForDeps :: Map ExeName (Set PackageName)
            -> Map PackageName PackageInfo
            -> Set Component
            -> BuildPlan
            -> PackageInfo
            -> IO a
            -> IO a
waitForDeps toolMap packageMap activeComps bp pi action = do
    atomically $ do
        mapM_ checkPackage $ addCabal $ Map.keysSet $ filterUnused $ sdPackages $ ppDesc $ piPlan pi
        forM_ (Map.keys $ filterUnused $ sdTools $ ppDesc $ piPlan pi) $ \exe -> do
            case lookup exe toolMap >>= fromNullable . map checkPackage . setToList of
                Nothing
                    | isCoreExe exe -> return ()
                    -- https://github.com/jgm/zip-archive/issues/23
                    -- - | otherwise -> throwSTM $ ToolMissing exe
                    | otherwise -> return ()
                Just packages -> ofoldl1' (<|>) packages
    action
  where
    filterUnused :: Ord key => Map key DepInfo -> Map key DepInfo
    filterUnused =
        mapFromList . filter (go . snd) . mapToList
      where
        go = not . null . intersection activeComps . diComponents

    checkPackage package | package == piName pi = return ()
    checkPackage package =
        case lookup package packageMap of
            Nothing
                | isCore package -> return ()
                | otherwise -> throwSTM $ DependencyMissing package
            Just dep -> do
                res <- readTMVar $ piResult dep
                unless res $ throwSTM $ DependencyFailed package

    isCore = (`member` siCorePackages (bpSystemInfo bp))
    isCoreExe = (`member` siCoreExecutables (bpSystemInfo bp))

    -- Since we build every package using the Cabal library, it's an implicit
    -- dependency of everything
    addCabal :: Set PackageName -> Set PackageName
    addCabal =
      case ppSimpleBuild $ piPlan pi of
        -- If we know that this is a simple build type, then we can
        -- build with the Cabal library that ships with GHC
        Just True -> id
        _ -> insertSet (mkPackageName "Cabal")

withCounter :: TVar Int -> IO a -> IO a
withCounter counter = bracket_
    (atomically $ modifyTVar counter (+ 1))
    (atomically $ modifyTVar counter (subtract 1))

data TSem = TSem !(TVar Int) !Int

newTSemIO :: Int -> IO TSem
newTSemIO x = flip TSem x <$> newTVarIO x

withTSem :: Bool -- ^ take all resources?
         -> TSem
         -> IO a
         -> IO a
withTSem useAll (TSem var total) inner = bracket
  (atomically $ do
      avail <- readTVar var
      let needed = if useAll then total else 1
      checkSTM $ avail >= needed
      writeTVar var $! avail - needed
      return needed)
  (\needed -> atomically $ modifyTVar var (+ needed))
  (const inner)

-- | Returns @Nothing@ if installing to a global database
pbDatabase :: PerformBuild -> Maybe FilePath
pbDatabase pb
    | pbGlobalInstall pb = Nothing
    | otherwise = Just $ pbInstallDest pb </> "pkgdb"

pbBinDir, pbLibDir, pbDataDir, pbLibexecDir, pbSysconfDir, pbDocDir :: PerformBuild -> FilePath
pbBinDir pb = pbInstallDest pb </> "bin"
pbLibDir pb = pbInstallDest pb </> "lib"
pbDataDir pb = pbInstallDest pb </> "share"
pbLibexecDir pb = pbInstallDest pb </> "libexec"
pbSysconfDir pb = pbInstallDest pb </> "etc"
pbDocDir pb = pbInstallDest pb </> "doc"

-- | Directory keeping previous result info
pbPrevResDir :: PerformBuild -> FilePath
pbPrevResDir pb = pbInstallDest pb </> "prevres"

performBuild :: PerformBuild -> IO [Text]
performBuild pb = do
    cwd <- getWorkingDirectory
    performBuild' pb
        { pbInstallDest = F.encodeString cwd </> pbInstallDest pb
        , pbLogDir = F.encodeString cwd </> pbLogDir pb
        }

performBuild' :: PerformBuild -> IO [Text]
performBuild' pb@PerformBuild {..} = withBuildDir $ \builddir -> do
    let removeTree' fp = whenM (doesDirectoryExist fp) (removeTree $ fromString fp)
    removeTree' $ fromString pbLogDir

    forM_ (pbDatabase pb) $ \db ->
        unlessM (doesFileExist $ db </> "package.cache") $ do
            createTree $ parent $ fromString db
            withCheckedProcess (proc "ghc-pkg" ["init", db])
                $ \ClosedStream Inherited Inherited -> return ()
    pbLog $ encodeUtf8 "Copying built-in Haddocks\n"
    copyBuiltInHaddocks (pbDocDir pb)

    sem <- newTSemIO pbJobs
    active <- newTVarIO (0 :: Int)
    let toolMap = makeToolMap (bpBuildToolOverrides pbPlan) (bpPackages pbPlan)

    errsVar <- newTVarIO mempty
    warningsVar <- newTVarIO id
    mutex <- newMVar ()
    env <- getEnvironment

    registeredPackages <- setupPackageDatabase
        (pbDatabase pb)
        (pbDocDir pb)
        pbLog
        (ppVersion <$> bpPackages pbPlan)
        (deletePreviousResults pb)
    allPreviousResults <- getAllPreviousResults pb

    packageMap' <- fmap fold $ forM (mapToList $ bpPackages pbPlan)
        $ \(name, plan) -> do
            let piPlan = plan
                piName = name
            piResult <- newEmptyTMVarIO
            return $ asMap $ singletonMap name PackageInfo {..}

    packageMap <- calculatePackageMap pb registeredPackages allPreviousResults packageMap'

    pbLog "Collecting existing .haddock files\n"
    haddockFiles <- getHaddockFiles pb >>= newTVarIO
    haddockDeps <- newTVarIO mempty

    forConcurrently_ packageMap $ \pi -> singleBuild pb registeredPackages
      SingleBuild
        { sbSem = sem
        , sbErrsVar = errsVar
        , sbWarningsVar = warningsVar
        , sbActive = active
        , sbToolMap = toolMap
        , sbPackageMap = packageMap
        , sbBuildDir = builddir
        , sbPackageInfo = pi
        , sbRegisterMutex = mutex
        , sbModifiedEnv = maybe
            id
            (\db -> (("HASKELL_PACKAGE_SANDBOX", db):))
            (pbDatabase pb)
            (filter allowedEnv $ map fixEnv env)
        , sbHaddockFiles = haddockFiles
        , sbHaddockDeps = haddockDeps
        }

    void $ tryAny $ atomically $ readTVar active >>= checkSTM . (== 0)

    warnings <- ($ []) <$> readTVarIO warningsVar
    errs <- readTVarIO errsVar
    when (not $ null errs) $ throwM $ BuildException errs warnings
    return warnings
  where
    withBuildDir f = withSystemTempDirectory "stackage-build" f

    fixEnv (p, x)
        -- Thank you Windows having case-insensitive environment variables...
        | toUpper p == "PATH" = (p, pbBinDir pb ++ pathSep : x)
        | otherwise = (p, x)

    allowedEnv (k, _) = k `notMember` bannedEnvs

    -- | Separate for the PATH environment variable
    pathSep :: Char
#ifdef mingw32_HOST_OS
    pathSep = ';'
#else
    pathSep = ':'
#endif

-- | Environment variables we don't allow to be passed on to child processes.
bannedEnvs :: Set String
bannedEnvs = setFromList
    [ "STACKAGE_AUTH_TOKEN"
    ]

data SingleBuild = SingleBuild
    { sbSem           :: TSem
    , sbErrsVar       :: TVar (Map PackageName BuildFailure)
    , sbWarningsVar   :: TVar ([Text] -> [Text])
    , sbActive        :: TVar Int
    , sbToolMap       :: Map ExeName (Set PackageName)
    , sbPackageMap    :: Map PackageName PackageInfo
    , sbBuildDir      :: FilePath
    , sbPackageInfo   :: PackageInfo
    , sbRegisterMutex :: MVar ()
    , sbModifiedEnv   :: [(String, String)]
    , sbHaddockFiles  :: TVar (Map Text FilePath) -- ^ package-version, .haddock file
    , sbHaddockDeps   :: TVar (Map PackageName (Set PackageName))
    -- ^ Deep deps of library and executables
    }

singleBuild :: PerformBuild
            -> Map PackageName Version -- ^ registered packages
            -> SingleBuild -> IO ()
singleBuild pb@PerformBuild {..} registeredPackages SingleBuild {..} = do
    withCounter sbActive
        $ handle updateErrs
        $ (`finally` void (atomically $ tryPutTMVar (piResult sbPackageInfo) False))
        $ inner
  where
    libComps = setFromList [CompLibrary, CompExecutable]
    testComps = insertSet CompTestSuite libComps
    benchComps = insertSet CompBenchmark libComps

    thisIsCabal = pname == mkPackageName "Cabal" -- cue Sparta joke

    inner
      | thisIsCabal && pbNoRebuildCabal =
            atomically $ putTMVar (piResult sbPackageInfo) True
      | otherwise = do
        let wfd comps =
                waitForDeps sbToolMap sbPackageMap comps pbPlan sbPackageInfo
                . withTSem pcNonParallelBuild sbSem
        withUnpacked <- wfd libComps buildLibrary

        wfd testComps (runTests withUnpacked)
        wfd benchComps (buildBenches withUnpacked)

    pname = piName sbPackageInfo
    pident = PackageIdentifier pname (ppVersion $ piPlan sbPackageInfo)
    name = display pname
    version = display $ ppVersion $ piPlan sbPackageInfo
    namever = concat
        [ name
        , "-"
        , version
        ]
    nameverrevhash = concat
        [ namever
        , fromMaybe (assert False "") $ do
            cfi <- ppCabalFileInfo $ piPlan sbPackageInfo
            hash <- lookup "SHA256" $ cfiHashes cfi
            Just $ "@sha256:" ++ hash
        ]

    quote :: Text -> Text
    quote s
        | any special s = tshow s
        | otherwise = s
      where
        special ' ' = True
        special '\'' = True
        special '"' = True
        special _ = False

    runIn :: FilePath -> IO Handle -> Text -> [Text] -> IO ()
    runIn wdir getOutH cmd args = do
        outH <- getOutH
        S.hPut outH $ encodeUtf8 $ concat
            [ "> "
            , pack wdir
            , "$ "
            , unwords $ map quote $ cmd : args
            , "\n"
            ]
        hFlush outH

        -- instead of using withCheckedProcess, we go lower-level so that we
        -- can kill the process in the case of an async exception (via the
        -- timeout call below)
        let cp' = cp outH
        (ClosedStream, UseProvidedHandle, UseProvidedHandle, sph)
            <- streamingProcess cp'
        ec <- waitForStreamingProcess sph `onException` do
            -- Call the process
            let ph = streamingProcessHandleRaw sph
            terminateProcess ph
        unless (ec == ExitSuccess) $ throwIO $ ProcessExitedUnsuccessfully cp' ec
      where
        cp outH = (proc (unpack cmd) (map unpack args))
            { cwd = Just wdir
            , std_out = UseHandle outH
            , std_err = UseHandle outH
            , env = Just sbModifiedEnv
            }
    runParent = runIn sbBuildDir

    log' t = do
        i <- readTVarIO sbActive
        errs <- readTVarIO sbErrsVar
        pbLog $ encodeUtf8 $ concat
            [ t
            , " (pending: "
            , tshow i
            , ", failures: "
            , tshow $ length errs
            , ")\n"
            ]
    libOut = pbLogDir </> unpack namever </> "build.out"
    testOut = pbLogDir </> unpack namever </> "test.out"
    benchOut = pbLogDir </> unpack namever </> "bench.out"

    wf fp inner' = do
        ref <- newIORef Nothing
        let cleanup = do
                mh <- readIORef ref
                forM_ mh hClose
            getH = do
                mh <- readIORef ref
                case mh of
                    Just h -> return h
                    Nothing -> mask_ $ do
                        createTree $ parent $ fromString fp
                        h <- openBinaryFile fp WriteMode
                        writeIORef ref $ Just h
                        return h

        inner' getH `finally` cleanup

    setup run args = do
        _ <- run "ghc" $ runghcArgs ["Setup"]
        run "./Setup" args
      where
        runghcArgs :: [Text] -> [Text]
        runghcArgs rest =
            "-clear-package-db"
            : "-global-package-db"
            : (case pbDatabase pb of
                Nothing -> rest
                Just db -> ("-package-db=" ++ pack db) : setupPackages ++ rest)

    setupPackages :: [Text]
    setupPackages =
        case sdSetupDeps $ ppDesc $ piPlan sbPackageInfo of
            Nothing -> []
            Just pkgs -> "-hide-all-packages" : map (("-package=" ++) . display) (setToList pkgs)

    ghcPkgArgs :: [Text] -> [Text]
    ghcPkgArgs rest =
          "--no-user-package-db"
        : (case pbDatabase pb of
            Nothing -> rest
            Just db -> ("--package-db=" ++ pack db) : rest)

    configArgs = ($ []) $ execWriter $ do
        tell' "--package-db=clear"
        tell' "--package-db=global"
        forM_ (pbDatabase pb) $ \db -> tell' $ "--package-db=" ++ pack db
        tell' $ "--libdir=" ++ pack (pbLibDir pb)
        tell' $ "--bindir=" ++ pack (pbBinDir pb)
        tell' $ "--datadir=" ++ pack (pbDataDir pb)
        tell' $ "--libexecdir=" ++ pack (pbLibexecDir pb)
        tell' $ "--sysconfdir=" ++ pack (pbSysconfDir pb)
        tell' $ "--docdir=" ++ pack (pbDocDir pb </> unpack namever)
        tell' $ "--htmldir=" ++ pack (pbDocDir pb </> unpack namever)
        tell' $ "--haddockdir=" ++ pack (pbDocDir pb </> unpack namever)
        when (not $ null flags) $ tell' $ "--flags=" ++ flags
        when (pbEnableLibProfiling && pcEnableLibProfile) $
            tell' "--enable-library-profiling"
        when pbEnableExecDyn $ tell' "--enable-executable-dynamic"

        tell (toList pcConfigureArgs ++)
      where
        tell' x = tell (x:)

    flags :: Text
    flags = pack $ unwords $ map go $ mapToList pcFlagOverrides
      where
        go (name', isOn) = concat
            [ if isOn then "" else "-"
            , unFlagName name'
            ]

    PackageConstraints {..} = ppConstraints $ piPlan sbPackageInfo

    hasLib = not $ null $ sdModules $ ppDesc $ piPlan sbPackageInfo

    buildLibrary = wf libOut $ \getOutH -> do
        gpdRef <- newIORef Nothing
        let withUnpacked reason inner' = do
                mgpd <- readIORef gpdRef
                (gpd, childDir) <-
                    case mgpd of
                        Just x -> return x
                        Nothing -> do
                            childDir <- if thisIsCabal && pbCabalFromHead
                                then do
                                    log' "Getting most recent Cabal from Git"
                                    runParent getOutH "git"
                                        [ "clone"
                                        , "https://github.com/haskell/cabal"
                                        ]
                                    return $ sbBuildDir </> "cabal" </> "Cabal"
                                else do
                                    log' $ "Unpacking " ++ nameverrevhash ++ " (reason: " ++ reason ++ ")"
                                    case ppSourceUrl $ piPlan sbPackageInfo of
                                        Nothing -> runParent getOutH "stack" ["unpack", nameverrevhash]
                                        Just url -> unpackFromURL sbBuildDir url
                                    return $ sbBuildDir </> unpack namever

                            gpd <- createSetupHs childDir name pbAllowNewer
                            writeIORef gpdRef $ Just (gpd, childDir)

                            return (gpd, childDir)
                inner' gpd childDir

        isConfiged <- newIORef False
        let withConfiged reason inner' = withUnpacked ("withConfiged: " ++ reason) $ \_gpd childDir -> do
                let run a b = do when pbVerbose $ log' (unwords (a : b))
                                 runIn childDir getOutH a b
                    cabal = setup run

                unlessM (readIORef isConfiged) $ do
                    log' $ "Configuring " ++ namever
                    cabal $ "configure" : configArgs
                    writeIORef isConfiged True
                inner' childDir cabal

        prevBuildResult <- getPreviousResult pb Build pident
        toBuild <- case () of
            ()
                | pcSkipBuild -> return False
                | prevBuildResult /= PRSuccess -> return True
                | pname `notMember` registeredPackages && hasLib -> do
                    log' $ concat
                        [ "WARNING: Package "
                        , display pname
                        , " marked as build success, but not registered"
                        ]
                    return True
                | otherwise -> return False
        when toBuild $ withConfiged "toBuild" $ \childDir cabal -> do
            deletePreviousResults pb pident

            log' $ "Building " ++ namever
            cabal ["build"]

            log' $ "Copying/registering " ++ namever
            cabal ["copy"]
            withMVar sbRegisterMutex $ const $ do
                cabal ["register"]
                when pcHide $ do
                    log' $ "Hiding " ++ namever
                    runIn childDir getOutH "ghc-pkg" $ ghcPkgArgs ["hide", namever]

            savePreviousResult pb Build pident True

        -- Even if the tests later fail, we can allow other libraries to build
        -- on top of our successful results
        --
        -- FIXME do we need to wait to do this until after Haddocks build?
        -- otherwise, we could have a race condition and try to build a
        -- dependency's haddocks before this finishes
        atomically $ putTMVar (piResult sbPackageInfo) True

        prevHaddockResult <- getPreviousResult pb Haddock pident
        let needHaddock = pbEnableHaddock
                       && checkPrevResult prevHaddockResult pcHaddocks
                       && not (null $ sdModules $ ppDesc $ piPlan sbPackageInfo)
                       && not pcSkipBuild
        when needHaddock $ withConfiged "needHaddock" $ \childDir cabal -> do
            log' $ "Haddocks " ++ namever
            hfs <- readTVarIO sbHaddockFiles
            haddockDeps <- atomically $ getHaddockDeps pbPlan sbHaddockDeps pname
            -- See: https://github.com/commercialhaskell/stack/pull/1070/files
            (hyped, _, _) <- readProcessWithExitCode "haddock" ["--hyperlinked-source"] ""
            let hfsOpts = map hfOpt
                        $ filter ((`member` haddockDeps) . toPackageName . fst)
                        $ mapToList hfs
                toPackageName t =
                    case simpleParse t of
                        Just (PackageIdentifier x _) -> x
                        Nothing -> error $ "Invalid package identifier: " ++ unpack t
                hfOpt (pkgVer, hf) = concat
                    [ "--haddock-options=--read-interface="
                    , "../"
                    , pkgVer
                    , "/,"
                    , pack hf
                    ]
                args = ($ hfsOpts) $ execWriter $ do
                        let tell' x = tell (x:)
                        tell' "haddock"
                        tell' $ if hyped == ExitSuccess
                            then "--haddock-option=--hyperlinked-source"
                            else "--hyperlink-source"
                        tell' "--html"
                        when pbBuildHoogle $ tell' "--hoogle"
                        tell' "--html-location=../$pkg-$version/"

            eres <- tryAny $ cabal args

            forM_ eres $ \() -> do
                renameOrCopy
                    (childDir </> "dist" </> "doc" </> "html" </> unpack name)
                    (pbDocDir pb </> unpack namever)

                enewPath <- tryIO'
                          $ canonicalizePath
                          $ fromString
                          $ pbDocDir pb
                        </> unpack namever
                        </> unpack name <.> "haddock"
                case enewPath of
                    Left e -> warn $ tshow e
                    Right newPath -> atomically
                                   $ modifyTVar sbHaddockFiles
                                   $ insertMap namever (F.encodeString newPath)

            savePreviousResult pb Haddock pident $ either (const False) (const True) eres
            case (eres, pcHaddocks) of
                (Left e, ExpectSuccess) -> throwM e
                (Right (), ExpectFailure) -> warn $ namever ++ ": unexpected Haddock success"
                _ -> return ()

        return withUnpacked

    runTests withUnpacked = wf testOut $ \getOutH -> do
        prevTestResult <- getPreviousResult pb Test pident
        let needTest = pbEnableTests
                    && checkPrevResult prevTestResult pcTests
                    && not pcSkipBuild
            hasTests = not . null . condTestSuites
        when needTest $ withUnpacked "needTest" $ \gpd childDir -> do
          -- If there are tests, run them. If not, then return `Right
          -- ()`. This was, we'll still save a successful test result
          -- and avoid re-unpacking next time around.
          eres <- if hasTests gpd
            then do
              let run = runIn childDir getOutH
                  cabal = setup run

              log' $ "Test configure " ++ namever
              cabal $ "configure" : "--enable-tests" : configArgs

              tryAny $ do
                  log' $ "Test build " ++ namever
                  cabal ["build"]

                  let tests = map (unUnqualComponentName . fst) $ condTestSuites gpd
                  forM_ tests $ \test -> do
                      log' $ concat
                          [ "Test run "
                          , namever
                          , " ("
                          , pack test
                          , ")"
                          ]
                      let exe = "dist/build" </> test </> test

                      exists <- liftIO $ doesFileExist $ childDir </> exe
                      if exists
                          then do
                              mres <- timeout maximumTestSuiteTime $ run (pack exe) []
                              case mres of
                                  Just () -> return ()
                                  Nothing -> error $ concat
                                      [ "Test suite timed out: "
                                      , unpack namever
                                      , ":"
                                      , test
                                      ]
                          else do
                              outH <- getOutH
                              hPut outH $ encodeUtf8 $ asText $ "Test suite not built: " ++ pack test
            else return $ Right ()

          savePreviousResult pb Test pident $ either (const False) (const True) eres
          case (eres, pcTests) of
            (Left e, ExpectSuccess) -> throwM e
            (Right (), ExpectFailure) -> warn $ namever ++ ": unexpected test success"
            _ -> return ()

    buildBenches withUnpacked = wf benchOut $ \getOutH -> do
        prevBenchResult <- getPreviousResult pb Bench pident
        let needBench = pbEnableBenches
                    && checkPrevResult prevBenchResult pcBenches
                    && not pcSkipBuild
            hasBenches = not . null . condBenchmarks
        when needBench $ withUnpacked "needBench" $ \gpd childDir -> do
          -- See explanation for this above in the test section
          eres <-
            if hasBenches gpd
              then do
                let run = runIn childDir getOutH
                    cabal = setup run

                log' $ "Benchmark configure " ++ namever
                cabal $ "configure" : "--enable-benchmarks" : configArgs

                tryAny $ do
                    log' $ "Benchmark build " ++ namever
                    cabal ["build"]

              else return $ Right ()

          savePreviousResult pb Bench pident $ either (const False) (const True) eres
          case (eres, pcBenches) of
            (Left e, ExpectSuccess) -> throwM e
            (Right (), ExpectFailure) -> warn $ namever ++ ": unexpected benchmark success"
            _ -> return ()


    warn t = atomically $ modifyTVar sbWarningsVar (. (t:))

    updateErrs exc = do
        log' $ concat
            [ display (piName sbPackageInfo)
            , ": "
            , take 500 $ tshow exc
            ]
        atomically $ modifyTVar sbErrsVar $ insertMap (piName sbPackageInfo) exc'
      where
        exc' =
            case fromException exc of
                Just bf -> bf
                Nothing -> BuildFailureException exc

-- | Unpack the file at the given URL into the given directory
unpackFromURL :: MonadIO m
              => FilePath -- ^ dest directory
              -> Text -- ^ URL
              -> m ()
unpackFromURL destDir url = liftIO $ do
    req <- parseRequest $ unpack url
    withSystemTempFile "unpack-from-url.tar.gz" $ \fp h -> do
      httpSink req (const $ sinkHandle h)
      hClose h
      let cp = (proc "tar" ["xf", fp])
                  { cwd = Just destDir
                  }
      withCheckedProcessCleanup cp
        $ \ClosedStream ClosedStream ClosedStream -> return ()

-- | Maximum time (in microseconds) to run a single test suite
maximumTestSuiteTime :: Int
maximumTestSuiteTime = 10 * 60 * 1000 * 1000 -- ten minutes

renameOrCopy :: FilePath -> FilePath -> IO ()
renameOrCopy src dest =
    rename (fromString src) (fromString dest)
    `catchIO'` \_ -> copyDir src dest

copyBuiltInHaddocks :: FilePath -> IO ()
copyBuiltInHaddocks docdir = do
    mghc <- findExecutable "ghc"
    case mghc of
        Nothing -> error "GHC not found on PATH"
        Just ghc -> do
            -- Starting with GHC 8, the doc/ghc directory is now
            -- doc/ghc-8.0.1 (and so on). Let's put in a hacky trick
            -- to find the right directory.

            let root = F.encodeString (parent (fromString ghc)) </>
                            "../share/doc"
            names <- getDirectoryContents root
            let hidden ('.':_) = True
                hidden _ = False
            name <-
                case filter (not . hidden) names of
                    [x] -> return x
                    _ -> error $ concat
                      [ "Unexpected list of contents in "
                      , root
                      , ": "
                      , show names
                      ]
            src <- canonicalizePath $ fromString $
                root </> name </> "html/libraries"
            copyDir (F.encodeString src) docdir

------------- Previous results

-- | The previous actions that can be run
data ResultType = Build | Haddock | Test | Bench
    deriving (Show, Enum, Eq, Ord, Bounded, Read)

-- | The result generated on a previous run
data PrevResult = PRNoResult | PRSuccess | PRFailure
    deriving (Show, Enum, Eq, Ord, Bounded, Read)

-- | Check if we should rerun based on a PrevResult and the expected status
checkPrevResult :: PrevResult -> TestState -> Bool
checkPrevResult _          Don'tBuild    = False
checkPrevResult PRNoResult _             = True
checkPrevResult PRSuccess  _             = False
checkPrevResult PRFailure  ExpectSuccess = True
checkPrevResult PRFailure  _             = False

withPRPath :: PerformBuild -> ResultType -> PackageIdentifier -> (FilePath -> IO a) -> IO a
withPRPath pb rt ident inner = do
    createTree $ parent $ fromString fp
    inner fp
  where
    fp = pbPrevResDir pb </> show rt </> unpack (display ident)

successBS, failureBS :: ByteString
successBS = "success"
failureBS = "failure"

getAllPreviousResults :: PerformBuild -> IO (PackageName -> ResultType -> Maybe (Version, PrevResult))
getAllPreviousResults pb = do
    m <- fmap fold $ forM [minBound..maxBound] $ \rt -> asMap . singletonMap rt <$> go rt
    return $ \pn rt -> lookup rt m >>= lookup pn
  where
    go :: ResultType -> IO (Map PackageName (Version, PrevResult))
    go rt = do
        exists <- doesDirectoryExist dir
        allResults <- if exists then
               runResourceT
             $ sourceDirectory dir
            $$ filterMC (liftIO . doesFileExist)
            =$ mapMC (liftIO . toMap)
            =$ foldlC (unionWith union) mempty
            else return mempty
        fmap concat $ mapM (uncurry removeDupes) $ mapToList allResults
      where
        dir = pbPrevResDir pb </> show rt

        toMap :: FilePath -> IO (Map PackageName (Map Version PrevResult))
        toMap fp = do
            case simpleParse $ pack $ FP.takeFileName fp of
                Nothing -> return mempty
                Just (PackageIdentifier name version) -> do
                    eres <- tryIO' $ readFile fp
                    let mres =
                            case eres of
                                Right bs
                                    | bs == successBS -> Just PRSuccess
                                    | bs == failureBS -> Just PRFailure
                                _                     -> Nothing
                    case mres of
                        Nothing -> return mempty
                        Just res -> return $ singletonMap name (singletonMap version res)

        removeDupes :: PackageName -> Map Version PrevResult -> IO (Map PackageName (Version, PrevResult))
        removeDupes name m =
            case mapToList m of
                [] -> assert False $ return mempty
                [pair] -> return $ singletonMap name pair
                _pairs -> do
                    removePreviousResults pb rt name
                    return mempty

getPreviousResult :: PerformBuild -> ResultType -> PackageIdentifier -> IO PrevResult
getPreviousResult w x y = withPRPath w x y $ \fp -> do
    eres <- tryIO' $ readFile fp
    return $ case eres of
        Right bs
            | bs == successBS -> PRSuccess
            | bs == failureBS -> PRFailure
        _                     -> PRNoResult

-- | Remove all previous results to avoid a broken cache
removePreviousResults :: PerformBuild -> ResultType -> PackageName -> IO ()
removePreviousResults pb rt name =
          whenM (doesDirectoryExist dir)
        $ runResourceT
        $ sourceDirectory dir
       $$ filterC isOurPackage
       =$ filterMC (liftIO . doesFileExist)
       =$ mapM_C (liftIO . System.Directory.removeFile)
  where
    dir = pbPrevResDir pb </> show rt
    prefix = display name ++ "-"

    isOurPackage fp = fromMaybe False $ do
        versionT <- stripPrefix prefix $ pack $ FP.takeFileName fp
        _ :: Version <- simpleParse versionT
        return True

savePreviousResult :: PerformBuild -> ResultType -> PackageIdentifier -> Bool -> IO ()
savePreviousResult pb rt ident@(PackageIdentifier name _version) res =
    withPRPath pb rt ident $ \fp -> do
        removePreviousResults pb rt name
        writeFile fp $ if res then successBS else failureBS

deletePreviousResults :: PerformBuild -> PackageIdentifier -> IO ()
deletePreviousResults pb name =
    forM_ [minBound..maxBound] $ \rt ->
    withPRPath pb rt name $ \fp ->
    void $ tryIO' $ removeFile $ fromString fp

-- | Discover existing .haddock files in the docs directory
getHaddockFiles :: PerformBuild -> IO (Map Text FilePath)
getHaddockFiles pb =
      runResourceT
    $ sourceDirectory (pbDocDir pb)
   $$ foldMapMC (liftIO . go)
  where
    go :: FilePath -> IO (Map Text FilePath)
    go dir =
        case simpleParse nameVerText of
            Nothing -> return mempty
            Just (PackageIdentifier (unPackageName -> name) _) -> do
                let fp = dir </> name <.> "haddock"
                exists <- doesFileExist fp
                return $ if exists
                    then singletonMap nameVerText fp
                    else mempty
      where
        nameVerText = pack $ FP.takeFileName dir

getHaddockDeps :: BuildPlan
               -> TVar (Map PackageName (Set PackageName))
               -> PackageName
               -> STM (Set PackageName)
getHaddockDeps BuildPlan {..} var =
    go
  where
    go :: PackageName -> STM (Set PackageName)
    go name = do
        m <- readTVar var
        case lookup name m of
            Just res -> return res
            Nothing -> do
                -- First thing we do is put in a dummy value in the var for
                -- this package, to avoid the possibility of an infinite loop
                -- due to packages depending on themselves (which is in fact
                -- valid).
                modifyTVar var $ insertMap name mempty

                res' <- fmap fold $ mapM go $ setToList deps
                let res = deps ++ res'
                modifyTVar var $ insertMap name res
                return res
      where
        deps =
            case lookup name bpPackages of
                Nothing -> mempty
                Just PackagePlan {..} ->
                    asSet
                  $ setFromList
                  $ map fst
                  $ filter (isLibExe . snd)
                  $ mapToList
                  $ sdPackages ppDesc

    isLibExe DepInfo {..} =
        CompLibrary    `member` diComponents ||
        CompExecutable `member` diComponents

sdistFilePath :: IsString filepath
              => FilePath -- ^ stack directory
              -> Text -- ^ package name
              -> Text -- ^ package name
              -> filepath
sdistFilePath stackDir name version = fromString
    $ stackDir
  </> "indices"
  </> "Hackage"
  </> "packages"
  </> unpack name
  </> unpack version
  </> unpack (concat [name, "-", version, ".tar.gz"])

-- | Create a default Setup.hs file if the given directory is a simple build plan
--
-- Also deletes any Setup.lhs if necessary
createSetupHs :: FilePath
              -> Text -- ^ package name
              -> Bool -- ^ allow newer?
              -> IO GenericPackageDescription
createSetupHs dir name allowNewer = do
    bs <- readFile cabalFP
    gpd' <- gpdFromLBS cabalFP (fromStrict bs)
    gpd <-
        if allowNewer
            then do
                let gpd = stripVersionBounds gpd'
                writeGenericPackageDescription cabalFP gpd
                return gpd
            else return gpd'
    let simple = buildType (packageDescription gpd) == Simple
    when simple $ do
        _ <- tryIO' $ removeFile $ fromString setuplhs
        writeFile setuphs $ asByteString "import Distribution.Simple\nmain = defaultMain\n"
    return gpd
  where
    cabalFP = dir </> unpack name <.> "cabal"
    setuphs = dir </> "Setup.hs"
    setuplhs = dir </> "Setup.lhs"

-- | Strip all version bounds from a GenericPackageDescription
stripVersionBounds :: GenericPackageDescription -> GenericPackageDescription
stripVersionBounds = everywhere $ mkT $ \(Dependency name _) -> Dependency name anyVersion

tryIO' :: IO a -> IO (Either IOException a)
tryIO' = try

catchIO' :: IO a -> (IOException -> IO a) -> IO a
catchIO' = catch

data BuildState = BSFullBuild | BSPartialBuild | NoBuild
    deriving Show

calculatePackageMap :: PerformBuild
                    -> Map PackageName Version -- ^ registered libraries
                    -> (PackageName -> ResultType -> Maybe (Version, PrevResult))
                    -> Map PackageName PackageInfo
                    -> IO (Map PackageName PackageInfo)
calculatePackageMap pb registered prevRes allInfos =
    loop initBuildStates
  where
    -- Calculate initial build states based on whether packages are
    -- registered and previous results. This will not take into
    -- account dependencies, which will be calculate by loop
    initBuildStates :: Map PackageName BuildState
    initBuildStates =
        foldMap go $ mapToList allInfos
      where
        go :: (PackageName, PackageInfo) -> Map PackageName BuildState
        go (name, info)
            -- If we're not going to build it, then don't build it
            | pcSkipBuild (ppConstraints plan) = singletonMap name NoBuild

            -- If this is a library and it's not registered, then we
            -- need to do a full build
            | isLib && lookup name registered /= Just version = singletonMap name BSFullBuild

            -- If we have a previous successful build result which
            -- matches our version, then we'll need to check
            -- dependencies to know what to do
            | Just (prevVer, prevRes') <- prevRes name Build
            , prevVer == version && prevRes' == PRSuccess = mempty

            -- Something in the previous step failed, so do a full build
            | otherwise = singletonMap name BSFullBuild
          where
            isLib = not $ null $ sdModules $ ppDesc plan
            version = ppVersion plan
            plan = piPlan info

    loop buildStates0 = do
        buildStates1 <- foldM step' buildStates0 (mapToList allInfos)
        when False $ putStrLn $ concat
            [ "Debugging: added "
            , tshow $ length buildStates1 - length buildStates0
            , " keys, new keys == "
            , tshow (map display $ keys $ buildStates1 `Map.difference` buildStates0)
            ]
        case (keys $ buildStates1 `difference` buildStates0, keys $ allInfos `Map.difference` buildStates1) of
            -- Added new build states, and no infos are unaccounted for, so
            -- we're done
            (_:_, []) -> processBuildStates buildStates1

            -- Added new build states, but we still have some unaccounted for
            -- packages, so loop
            (_:_, _:_) -> loop buildStates1

            ([], []) -> processBuildStates buildStates1

            {- FIXME make this testing more robust
            (_:_, _:_) -> loop buildStates1

            -- Did not add any build states, but somehow all the infos are
            -- accounted for. This is logically impossible, print an error.
            ([], []) -> do
                putStrLn $ "\n\ncalculatePackageMap: Solved all packages, but no change in build states"
                mapM_ print $ mapToList buildStates1
                putStrLn $ "\n\nPreviously missing packages:" ++ tshow (map display $ keys $ allInfos `Map.difference` buildStates0)
                putStrLn $ concat
                    [ "length allInfos: "
                    , tshow $ length allInfos
                    , ", length buildStates1: "
                    , tshow $ length buildStates1
                    , ", length buildStates0: "
                    , tshow $ length buildStates0
                    ]
                error "FIXME"
            -}

            -- Did not add any build states, and we still have some infos
            -- unaccounted for. That indicates some kind of cyclic dependency.
            ([], noBuildState) -> error $ "calculatePackageMap: No change in build states, but haven't solved all packages: " ++ show (map display noBuildState)
      where
        step' buildStates (name, info) = do
            res <- step buildStates name info
            return $
                case res of
                    Nothing -> buildStates
                    Just bs -> insertMap name bs buildStates

    step :: Map PackageName BuildState -> PackageName -> PackageInfo -> IO (Maybe BuildState)
    step states name pi =
        go $ keys $ Map.filter ((CompLibrary `member`) . diComponents) $ sdPackages desc
      where
        plan = piPlan pi
        desc = ppDesc plan

        go (dep:deps) | dep == name = go deps
        go (dep:deps) =
            case lookup dep states of
                -- Don't know what to do with a dep. Let's see if it's
                -- actually in the build plan (as opposed to a core
                -- library)
                Nothing
                    -- It's in the build plan, so let's wait
                    | dep `member` allInfos -> do
                        when False $ putStrLn $ concat -- debugging
                            [ display name
                            , ": don't know state of dep: "
                            , tshow dep
                            ]
                        return Nothing

                    -- Not in the build plan, so ignore and move on
                    -- (e.g., the base package)
                    | otherwise -> go deps

                -- dep will be rebuilt, so we need to be rebuilt too
                Just BSFullBuild -> do
                    putStrLn $ concat
                        [ "Rebuilding "
                        , display name
                        , " due to dependency "
                        , display dep
                        ]
                    return $ Just BSFullBuild

                -- dep will not have its library rebuilt, so check the rest
                Just BSPartialBuild -> go deps
                Just NoBuild -> go deps

        -- none of the dependencies will have their libraries rebuilt
        go [] = return $ Just BSPartialBuild -- FIXME figure out if we have no need to build at all NoBuild

    processBuildStates :: Map PackageName BuildState -> IO (Map PackageName PackageInfo)
    processBuildStates buildStates =
        fmap fold $ mapM go $ mapToList allInfos
      where
        go :: (PackageName, PackageInfo) -> IO (Map PackageName PackageInfo)
        go (name, info) =
            case lookup name buildStates of
                Nothing -> error $ "processBuildStates: name not found " ++ show name
                Just BSFullBuild -> do
                    putStrLn $ "Removing all previous results for " ++ display name
                    forM_ [minBound..maxBound] $ \rt -> removePreviousResults pb rt name
                    return $ singletonMap name info
                Just BSPartialBuild -> return $ singletonMap name info
                Just NoBuild -> return mempty
