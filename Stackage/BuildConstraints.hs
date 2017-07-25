{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE GADTs             #-}
-- | The constraints on package selection for a new build plan.
module Stackage.BuildConstraints
    ( BuildConstraints (..)
    , PackageConstraints (..)
    , TestState (..)
    , SystemInfo (..)
    , getSystemInfo
    , defaultBuildConstraints
    , toBC
    , BuildConstraintsSource (..)
    , loadBuildConstraints
    , setConstraints
    ) where

import           Control.Monad.Writer.Strict (execWriter, tell)
import           Data.Aeson
import           Data.Aeson.Internal         ((<?>), JSONPathElement (Key))
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Yaml                   (decodeEither', decodeFileEither)
import           Distribution.Package        (Dependency (..))
import qualified Distribution.System
import           Distribution.Version        (anyVersion)
import           Network.HTTP.Client         (Manager, httpLbs, responseBody, Request)
import           Stackage.CorePackages
import           Stackage.Prelude
import           System.Directory            (doesFileExist)

data BuildConstraints = BuildConstraints
    { bcPackages           :: Set PackageName
    -- ^ This does not include core packages.
    , bcPackageConstraints :: PackageName -> PackageConstraints

    , bcSystemInfo         :: SystemInfo

    , bcGithubUsers        :: Map Text (Set Text)
    -- ^ map an account to set of pingees

    , bcBuildToolOverrides :: Map Text (Set Text)
    -- ^ map a build tool name to a set of packages we should include
    --
    -- Used to avoid situations like extra packages on Hackage providing the
    -- cabal executable

    , bcTellMeWhenItsReleased :: Map PackageName Version

    , bcNoRevisions :: !(Set PackageName)
    -- ^ see 'cfNoRevisions'
    }

-- | Modify the version bounds with the given Dependencies
setConstraints :: [Dependency] -> BuildConstraints -> BuildConstraints
setConstraints deps bc =
    bc { bcPackageConstraints = f }
  where
    depMap = unionsWith intersectVersionRanges $ map toMap deps
    toMap (Dependency k v) = asMap $ singletonMap k v

    f' = bcPackageConstraints bc
    f pkg =
        case lookup pkg depMap of
            Nothing -> pc
            Just vr -> pc { pcVersionRange = vr }
      where
        pc = f' pkg

-- | The proposed plan from the requirements provided by contributors.
--
-- Checks the current directory for a build-constraints.yaml file and uses it
-- if present. If not, downloads from Github.
defaultBuildConstraints :: Manager -> IO BuildConstraints
defaultBuildConstraints = loadBuildConstraints BCSDefault

data BuildConstraintsSource
    = BCSDefault
    | BCSFile FilePath
    | BCSWeb Request
    deriving (Show)

loadBuildConstraints :: BuildConstraintsSource -> Manager -> IO BuildConstraints
loadBuildConstraints bcs man = do
    case bcs of
        BCSDefault -> do
            e <- doesFileExist fp0
            if e
                then loadFile fp0
                else loadReq req0
        BCSFile fp -> loadFile fp
        BCSWeb req -> loadReq req
  where
    fp0 = "build-constraints.yaml"
    req0 = "https://raw.githubusercontent.com/fpco/stackage/master/build-constraints.yaml"

    loadFile fp = decodeFileEither fp >>= either throwIO toBC
    loadReq req = httpLbs req man >>=
                  either throwIO toBC . decodeEither' . toStrict . responseBody


getSystemInfo :: IO SystemInfo
getSystemInfo = do
    siCorePackages <- getCorePackages
    siCoreExecutables <- getCoreExecutables
    siGhcVersion <- getGhcVersion
    return SystemInfo {..}
  where
    -- FIXME consider not hard-coding the next two values
    siOS   = Distribution.System.Linux
    siArch = Distribution.System.X86_64

data ConstraintFile = ConstraintFile
    { cfPackageFlags            :: Map PackageName (Map FlagName Bool)
    , cfConfigureArgs           :: Map PackageName (Vector Text)
    , cfSkippedTests            :: Set PackageName
    , cfSkippedBuilds           :: Set PackageName
    , cfExpectedTestFailures    :: Set PackageName
    , cfExpectedBenchFailures   :: Set PackageName
    , cfExpectedHaddockFailures :: Set PackageName
    , cfSkippedBenchmarks       :: Set PackageName
    , cfPackages                :: Map Maintainer (Vector Dependency)
    , cfGithubUsers             :: Map Text (Set Text)
    , cfBuildToolOverrides      :: Map Text (Set Text)
    , cfSkippedLibProfiling     :: Set PackageName
    , cfGhcMajorVersion         :: Maybe (Int, Int)
    , cfTreatAsNonCore          :: Set PackageName
    , cfTellMeWhenItsReleased   :: Map PackageName Version
    , cfHide                    :: Set PackageName
    -- ^ Packages which should be hidden after registering
    , cfNoRevisions             :: !(Set PackageName)
    -- ^ Packages where we should ignore any Hackage revisions
    }

instance FromJSON ConstraintFile where
    parseJSON = withObject "ConstraintFile" $ \o -> do
        cfPackageFlags <- (goPackageMap . fmap goFlagMap) <$> o .: "package-flags"
        cfConfigureArgs <- goPackageMap <$> o .:? "configure-args" .!= mempty
        cfSkippedTests <- getPackages o "skipped-tests"
        cfSkippedBuilds <- getPackages o "skipped-builds" <|> return mempty
        cfExpectedTestFailures <- getPackages o "expected-test-failures"
        cfExpectedBenchFailures <- getPackages o "expected-benchmark-failures"
                               <|> pure mempty -- backwards compat
        cfExpectedHaddockFailures <- getPackages o "expected-haddock-failures"
        cfSkippedBenchmarks <- getPackages o "skipped-benchmarks"
        cfSkippedLibProfiling <- getPackages o "skipped-profiling"
        cfPackages <- o .: "packages"
                  >>= mapM (mapM toDep)
                    . Map.mapKeysWith const Maintainer
        cfGithubUsers <- o .: "github-users"
        cfBuildToolOverrides <- o .:? "build-tool-overrides" .!= mempty
        cfGhcMajorVersion <- o .:? "ghc-major-version" >>= mapM parseMajorVersion
        cfTreatAsNonCore <- getPackages o "treat-as-non-core" <|> return mempty
        cfTellMeWhenItsReleased <- (fmap mconcat $ o .: "tell-me-when-its-released" >>= mapM toNameVerMap)
                               <?> Key "tell-me-when-its-released"
        cfHide <- Set.map mkPackageName <$> o .:? "hide" .!= mempty
        cfNoRevisions <- Set.map mkPackageName <$> o .:? "no-revisions" .!= mempty
        return ConstraintFile {..}
      where
        goFlagMap = Map.mapKeysWith const mkFlagName
        goPackageMap = Map.mapKeysWith const mkPackageName
        getPackages o name = (setFromList . map mkPackageName) <$> o .: name

        toDep :: Monad m => Text -> m Dependency
        toDep = either (fail . show) return . simpleParse

        toNameVerMap :: Monad m => Text -> m (Map PackageName Version)
        toNameVerMap = either (fail . show) (\(PackageIdentifier x y) -> return $ singletonMap x y)
                     . simpleParse

        parseMajorVersion t =
            case versionNumbers <$> simpleParse t of
                Just [x, y] -> return (x, y)
                _ -> fail $ "Invalid GHC major version: " ++ unpack t

data MismatchedGhcVersion = MismatchedGhcVersion
    { _mgvGhcOnPath :: !Version
    , _mgvExpectedMajor :: !Int
    , _mgcExpectedMinor :: !Int
    }
    deriving (Show, Typeable)
instance Exception MismatchedGhcVersion

-- | Remove the given packages from the set of core packages
removeFromCore :: Set PackageName -> SystemInfo -> SystemInfo
removeFromCore forceNonCore si = si
    { siCorePackages = siCorePackages si
      `Map.difference` mapFromList (map (, ()) $ toList forceNonCore)
    }

toBC :: ConstraintFile -> IO BuildConstraints
toBC ConstraintFile {..} = do
    bcSystemInfo <- removeFromCore cfTreatAsNonCore <$> getSystemInfo
    forM_ cfGhcMajorVersion $ \(major, minor) ->
        case versionNumbers $ siGhcVersion bcSystemInfo of
            major':minor':_ | major == major' && minor == minor' -> return ()
            _ -> throwIO $ MismatchedGhcVersion (siGhcVersion bcSystemInfo) major minor
    return BuildConstraints {..}
  where
    combine (maintainer, range1) (_, range2) =
        (maintainer, intersectVersionRanges range1 range2)
    revmap = unionsWith combine $ ($ []) $ execWriter
           $ forM_ (mapToList cfPackages)
           $ \(maintainer, deps) -> forM_ deps
           $ \(Dependency name range) ->
            tell (singletonMap name (maintainer, range):)

    bcPackages = Map.keysSet revmap

    bcPackageConstraints name =
        PackageConstraints {..}
      where
        mpair = lookup name revmap
        pcMaintainer = fmap fst mpair
        pcVersionRange = maybe anyVersion snd mpair
        pcEnableLibProfile = not (name `member` cfSkippedLibProfiling)
        pcTests
            | name `member` cfSkippedTests = Don'tBuild
            | name `member` cfExpectedTestFailures = ExpectFailure
            | otherwise = ExpectSuccess
        pcBenches
            | name `member` cfSkippedBenchmarks = Don'tBuild
            | name `member` cfExpectedBenchFailures = ExpectFailure
            | otherwise = ExpectSuccess
        pcHaddocks
            | name `member` cfExpectedHaddockFailures = ExpectFailure

            | otherwise = ExpectSuccess
        pcFlagOverrides = fromMaybe mempty $ lookup name cfPackageFlags
        pcConfigureArgs = fromMaybe mempty $ lookup name cfConfigureArgs
        pcSkipBuild = name `member` cfSkippedBuilds
        pcHide = name `member` cfHide

    bcGithubUsers = cfGithubUsers
    bcBuildToolOverrides = cfBuildToolOverrides
    bcTellMeWhenItsReleased = cfTellMeWhenItsReleased
    bcNoRevisions = cfNoRevisions
