{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
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
    ) where

import           Control.Monad.Writer.Strict (execWriter, tell)
import           Data.Aeson
import qualified Data.Map                    as Map
import           Data.Yaml                   (decodeEither', decodeFileEither)
import           Distribution.Package        (Dependency (..))
import           Distribution.System         (Arch, OS)
import qualified Distribution.System
import           Distribution.Version        (anyVersion)
import           Filesystem                  (isFile)
import           Network.HTTP.Client         (Manager, httpLbs, responseBody, Request)
import           Stackage.CorePackages
import           Stackage.Prelude

data BuildConstraints = BuildConstraints
    { bcPackages           :: Set PackageName
    -- ^ This does not include core packages.
    , bcPackageConstraints :: PackageName -> PackageConstraints

    , bcSystemInfo         :: SystemInfo

    , bcGithubUsers        :: Map Text (Set Text)
    -- ^ map an account to set of pingees
    }

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
            e <- isFile fp0
            if e
                then loadFile fp0
                else loadReq req0
        BCSFile fp -> loadFile fp
        BCSWeb req -> loadReq req
  where
    fp0 = "build-constraints.yaml"
    req0 = "https://raw.githubusercontent.com/fpco/stackage/master/build-constraints.yaml"

    loadFile fp = decodeFileEither (fpToString fp) >>= either throwIO toBC
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
    , cfSkippedTests            :: Set PackageName
    , cfExpectedTestFailures    :: Set PackageName
    , cfExpectedHaddockFailures :: Set PackageName
    , cfSkippedBenchmarks       :: Set PackageName
    , cfPackages                :: Map Maintainer (Vector Dependency)
    , cfGithubUsers             :: Map Text (Set Text)
    , cfSkippedLibProfiling     :: Set PackageName
    }

instance FromJSON ConstraintFile where
    parseJSON = withObject "ConstraintFile" $ \o -> do
        cfPackageFlags <- (goPackageMap . fmap goFlagMap) <$> o .: "package-flags"
        cfSkippedTests <- getPackages o "skipped-tests"
        cfExpectedTestFailures <- getPackages o "expected-test-failures"
        cfExpectedHaddockFailures <- getPackages o "expected-haddock-failures"
        cfSkippedBenchmarks <- getPackages o "skipped-benchmarks"
        cfSkippedLibProfiling <- getPackages o "skipped-profiling"
        cfPackages <- o .: "packages"
                  >>= mapM (mapM toDep)
                    . Map.mapKeysWith const Maintainer
        cfGithubUsers <- o .: "github-users"
        return ConstraintFile {..}
      where
        goFlagMap = Map.mapKeysWith const FlagName
        goPackageMap = Map.mapKeysWith const PackageName
        getPackages o name = (setFromList . map PackageName) <$> o .: name

        toDep :: Monad m => Text -> m Dependency
        toDep = either (fail . show) return . simpleParse

toBC :: ConstraintFile -> IO BuildConstraints
toBC ConstraintFile {..} = do
    bcSystemInfo <- getSystemInfo
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
        pcBuildBenchmarks = name `notMember` cfSkippedBenchmarks
        pcHaddocks
            | name `member` cfExpectedHaddockFailures = ExpectFailure

            | otherwise = ExpectSuccess
        pcFlagOverrides = fromMaybe mempty $ lookup name cfPackageFlags

    bcGithubUsers = cfGithubUsers
