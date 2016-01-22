-- | Perform an actual build, generate a binary package database and a
-- documentation directory in the process.
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleContexts   #-}
module Stackage.PerformBuild
    ( performBuild
    , PerformBuild (..)
    , BuildException (..)
    , pbDocDir
    , sdistFilePath
    ) where

import           Control.Concurrent.Async    (async)
import           Control.Concurrent.STM.TSem
import           Control.Monad.Writer.Strict (execWriter, tell)
import qualified Data.Map                    as Map
import           Data.NonNull                (fromNullable)
import           Distribution.PackageDescription (buildType, packageDescription, BuildType (Simple),
                                                 condTestSuites)
import           Filesystem                  (canonicalizePath, createTree,
                                              getWorkingDirectory,
                                              removeTree, rename, removeFile)
import           Filesystem.Path             (parent)
import qualified Filesystem.Path.CurrentOS   as F
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.GhcPkg
import           Stackage.PackageDescription
import           Stackage.PackageIndex       (gpdFromLBS)
import           Stackage.Prelude            hiding (pi)
import           System.Directory            (doesDirectoryExist, doesFileExist, findExecutable,
                                              getAppUserDataDirectory)
import qualified System.FilePath             as FP
import           System.Environment          (getEnvironment)
import           System.Exit
import           System.IO                   (IOMode (WriteMode),
                                              openBinaryFile)
import           System.IO.Temp              (withSystemTempDirectory)

data BuildException = BuildException (Map PackageName BuildFailure) [Text]
    deriving Typeable
instance Exception BuildException
instance Show BuildException where
    show (BuildException m warnings) =
        unlines $ map go (mapToList m) ++ map unpack warnings
      where
        go (PackageName name, bf) = concat
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
    , pbEnableHaddock      :: Bool
    , pbEnableLibProfiling :: Bool
    , pbEnableExecDyn      :: Bool
    , pbVerbose            :: Bool
    , pbAllowNewer         :: Bool
    -- ^ Pass --allow-newer to cabal configure
    , pbBuildHoogle        :: Bool
    -- ^ Should we build Hoogle database?
    --
    -- May be disabled due to: https://ghc.haskell.org/trac/ghc/ticket/9921
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
        mapM_ checkPackage $ Map.keys $ filterUnused $ sdPackages $ ppDesc $ piPlan pi
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

withCounter :: TVar Int -> IO a -> IO a
withCounter counter = bracket_
    (atomically $ modifyTVar counter (+ 1))
    (atomically $ modifyTVar counter (subtract 1))

withTSem :: TSem -> IO a -> IO a
withTSem sem = bracket_ (atomically $ waitTSem sem) (atomically $ signalTSem sem)

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

    sem <- atomically $ newTSem pbJobs
    active <- newTVarIO (0 :: Int)
    let toolMap = makeToolMap (bpBuildToolOverrides pbPlan) (bpPackages pbPlan)
    packageMap <- fmap fold $ forM (mapToList $ bpPackages pbPlan)
        $ \(name, plan) -> do
            let piPlan = plan
                piName = name
            piResult <- newEmptyTMVarIO
            return $ singletonMap name PackageInfo {..}

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

    pbLog "Collecting existing .haddock files\n"
    haddockFiles <- getHaddockFiles pb >>= newTVarIO
    haddockDeps <- newTVarIO mempty

    forM_ packageMap $ \pi -> void $ async $ singleBuild pb registeredPackages
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
            -> Set PackageName -- ^ registered packages
            -> SingleBuild -> IO ()
singleBuild pb@PerformBuild {..} registeredPackages SingleBuild {..} = do
    withCounter sbActive
        $ handle updateErrs
        $ (`finally` void (atomically $ tryPutTMVar (piResult sbPackageInfo) False))
        $ inner
  where
    libComps = setFromList [CompLibrary, CompExecutable]
    testComps = insertSet CompTestSuite libComps
    inner = do
        let wfd comps =
                waitForDeps sbToolMap sbPackageMap comps pbPlan sbPackageInfo
                . withTSem sbSem
        withUnpacked <- wfd libComps buildLibrary

        wfd testComps (runTests withUnpacked)

    pname = piName sbPackageInfo
    pident = PackageIdentifier pname (ppVersion $ piPlan sbPackageInfo)
    name = display pname
    version = display $ ppVersion $ piPlan sbPackageInfo
    namever = concat
        [ name
        , "-"
        , version
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
        hPutStrLn outH $ concat
            [ "> "
            , pack wdir
            , "$ "
            , unwords $ map quote $ cmd : args
            ]
        withCheckedProcess (cp outH) $ \ClosedStream UseProvidedHandle UseProvidedHandle ->
            (return () :: IO ())
      where
        cp outH = (proc (unpack cmd) (map unpack args))
            { cwd = Just wdir
            , std_out = UseHandle outH
            , std_err = UseHandle outH
            , env = Just sbModifiedEnv
            }
    runParent = runIn sbBuildDir
    runChild = runIn childDir
    childDir = sbBuildDir </> unpack namever

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

    runghcArgs :: [Text] -> [Text]
    runghcArgs rest =
          "-clear-package-db"
        : "-global-package-db"
        : (case pbDatabase pb of
            Nothing -> rest
            Just db -> ("-package-db=" ++ pack db) : rest)

    configArgs = ($ []) $ execWriter $ do
        when pbAllowNewer $ tell' "--allow-newer"
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
        tell' $ "--flags=" ++ flags
        when (pbEnableLibProfiling && pcEnableLibProfile) $
            tell' "--enable-library-profiling"
        when pbEnableExecDyn $ tell' "--enable-executable-dynamic"
      where
        tell' x = tell (x:)

    flags :: Text
    flags = unwords $ map go $ mapToList pcFlagOverrides
      where
        go (name', isOn) = concat
            [ if isOn then "" else "-"
            , unFlagName name'
            ]

    PackageConstraints {..} = ppConstraints $ piPlan sbPackageInfo

    hasLib = not $ null $ sdModules $ ppDesc $ piPlan sbPackageInfo

    buildLibrary = wf libOut $ \getOutH -> do
        let run a b = do when pbVerbose $ log' (unwords (a : b))
                         runChild getOutH a b
            cabal args = run "runghc" $ runghcArgs $ "Setup" : args

        gpdRef <- newIORef Nothing
        let withUnpacked inner' = do
                mgpd <- readIORef gpdRef
                gpd <-
                    case mgpd of
                        Just gpd -> return gpd
                        Nothing -> do
                            log' $ "Unpacking " ++ namever
                            runParent getOutH "stack" ["unpack", namever]

                            gpd <- createSetupHs childDir name
                            writeIORef gpdRef $ Just gpd

                            return gpd
                inner' gpd

        isConfiged <- newIORef False
        let withConfiged inner' = withUnpacked $ \_gpd -> do
                unlessM (readIORef isConfiged) $ do
                    log' $ "Configuring " ++ namever
                    cabal $ "configure" : configArgs
                    writeIORef isConfiged True
                inner'

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
        when toBuild $ withConfiged $ do
            deletePreviousResults pb pident

            log' $ "Building " ++ namever
            cabal ["build"]

            log' $ "Copying/registering " ++ namever
            cabal ["copy"]
            withMVar sbRegisterMutex $ const $
                cabal ["register"]

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
        when needHaddock $ withConfiged $ do
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

                enewPath <- tryIO
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
        let run = runChild getOutH
            cabal args = run "runghc" $ runghcArgs $ "Setup" : args

        prevTestResult <- getPreviousResult pb Test pident
        let needTest = pbEnableTests
                    && checkPrevResult prevTestResult pcTests
                    && not pcSkipBuild
        when needTest $ withUnpacked $ \gpd -> do
            log' $ "Test configure " ++ namever
            cabal $ "configure" : "--enable-tests" : configArgs

            eres <- tryAny $ do
                log' $ "Test build " ++ namever
                cabal ["build"]

                let tests = map fst $ condTestSuites gpd
                forM_ tests $ \test -> do
                    log' $ concat
                        [ "Test run "
                        , namever
                        , " ("
                        , pack test
                        , ")"
                        ]
                    let exe = pack $ "dist/build" </> test </> test
                    -- FIXME: may want to put in some logic to test if the
                    -- executable exists
                    run exe []

            savePreviousResult pb Test pident $ either (const False) (const True) eres
            case (eres, pcTests) of
                (Left e, ExpectSuccess) -> throwM e
                (Right (), ExpectFailure) -> warn $ namever ++ ": unexpected test success"
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

renameOrCopy :: FilePath -> FilePath -> IO ()
renameOrCopy src dest =
    rename (fromString src) (fromString dest)
    `catchIO` \_ -> copyDir src dest

copyBuiltInHaddocks :: FilePath -> IO ()
copyBuiltInHaddocks docdir = do
    mghc <- findExecutable "ghc"
    case mghc of
        Nothing -> error "GHC not found on PATH"
        Just ghc -> do
            src <- canonicalizePath $ fromString
                (F.encodeString (parent (fromString ghc)) </> "../share/doc/ghc/html/libraries")
            copyDir (F.encodeString src) docdir

------------- Previous results

-- | The previous actions that can be run
data ResultType = Build | Haddock | Test
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

getPreviousResult :: PerformBuild -> ResultType -> PackageIdentifier -> IO PrevResult
getPreviousResult w x y = withPRPath w x y $ \fp -> do
    eres <- tryIO $ readFile fp
    return $ case eres of
        Right bs
            | bs == successBS -> PRSuccess
            | bs == failureBS -> PRFailure
        _                     -> PRNoResult

savePreviousResult :: PerformBuild -> ResultType -> PackageIdentifier -> Bool -> IO ()
savePreviousResult pb rt ident res =
    withPRPath pb rt ident $ \fp -> writeFile fp $
        if res then successBS else failureBS

deletePreviousResults :: PerformBuild -> PackageIdentifier -> IO ()
deletePreviousResults pb name =
    forM_ [minBound..maxBound] $ \rt ->
    withPRPath pb rt name $ \fp ->
    void $ tryIO $ removeFile $ fromString fp

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
            Just (PackageIdentifier (PackageName name) _) -> do
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
              -> IO GenericPackageDescription
createSetupHs dir name = do
    bs <- readFile cabalFP
    gpd <- gpdFromLBS cabalFP (fromStrict bs)
    let simple = buildType (packageDescription gpd) == Just Simple
    when simple $ do
        _ <- tryIO $ removeFile $ fromString setuplhs
        writeFile setuphs $ asByteString "import Distribution.Simple\nmain = defaultMain\n"
    return gpd
  where
    cabalFP = dir </> unpack name <.> "cabal"
    setuphs = dir </> "Setup.hs"
    setuplhs = dir </> "Setup.lhs"
