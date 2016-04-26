{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- | Shared types for various stackage packages.
module Stackage.Types
    ( -- * Types
      SnapshotType (..)
    , DocMap
    , PackageDocs (..)
    , BuildPlan (..)
    , PackagePlan (..)
    , PackageConstraints (..)
    , ParseFailedException (..)
    , TestState (..)
    , SystemInfo (..)
    , Maintainer (..)
    , ExeName (..)
    , SimpleDesc (..)
    , DepInfo (..)
    , Component (..)
    , CabalFileInfo (..)
      -- * Helper functions
    , display
    , simpleParse
    , unPackageName
    , mkPackageName
    , unFlagName
    , mkFlagName
    , intersectVersionRanges
    ) where

import           Control.Applicative             ((<$>), (<*>), (<|>))
import           Control.Arrow                   ((&&&))
import           Control.Exception               (Exception)
import           Control.Monad.Catch             (MonadThrow, throwM)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  object, withObject, withText,
                                                  (.!=), (.:), (.:?), (.=))
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (Monoid, mappend, mempty)
import           Data.Semigroup                  (Semigroup, (<>))
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.String                     (IsString, fromString)
import           Data.Text                       (Text, pack, unpack)
import qualified Data.Text                       as T
import           Data.Time                       (Day)
import qualified Data.Traversable                as T
import           Data.Typeable                   (TypeRep, Typeable, typeOf)
import           Data.Vector                     (Vector)
import           Distribution.Package            (PackageName (PackageName))
import           Distribution.PackageDescription (FlagName (..))
import           Distribution.System             (Arch, OS)
import qualified Distribution.Text               as DT
import           Distribution.Version            (Version, VersionRange)
import qualified Distribution.Version            as C
import Safe (readMay)
import qualified Data.Binary                           as Bin (Binary)
import qualified Data.Binary.Tagged                    as Bin
import qualified Data.ByteString.Base16                as B16
import GHC.Generics (Generic)

data SnapshotType = STNightly
                  | STNightly2 !Day
                  | STLTS !Int !Int -- ^ major, minor
    deriving (Show, Read, Eq, Ord)

instance ToJSON SnapshotType where
    toJSON STNightly = object
        [ "type" .= asText "nightly"
        ]
    toJSON (STNightly2 day) = object
        [ "type" .= asText "nightly"
        , "date" .= show day
        ]
    toJSON (STLTS major minor) = object
        [ "type" .= asText "lts"
        , "major" .= major
        , "minor" .= minor
        ]
instance FromJSON SnapshotType where
    parseJSON = withObject "SnapshotType" $ \o -> do
        t <- o .: "type"
        case asText t of
            "nightly" -> (STNightly2 <$> (o .: "date" >>= readFail)) <|> return STNightly
            "lts" -> STLTS
                <$> o .: "major"
                <*> o .: "minor"
            _ -> fail $ "Unknown type for SnapshotType: " ++ unpack t
      where
        readFail t =
            case readMay t of
                Nothing -> fail "read failed"
                Just x -> return x

-- | Package name is key
type DocMap = Map Text PackageDocs

asText :: Text -> Text
asText = id

data PackageDocs = PackageDocs
    { pdVersion :: Text
    , pdModules :: Map Text [Text]
    -- ^ module name, path
    }
instance ToJSON PackageDocs where
    toJSON PackageDocs {..} = object
        [ "version" .= pdVersion
        , "modules" .= pdModules
        ]
instance FromJSON PackageDocs where
    parseJSON = withObject "PackageDocs" $ \o -> PackageDocs
        <$> o .: "version"
        <*> o .: "modules"

data BuildPlan = BuildPlan
    { bpSystemInfo  :: SystemInfo
    , bpTools       :: Vector (PackageName, Version)
    , bpPackages    :: Map PackageName PackagePlan
    , bpGithubUsers :: Map Text (Set Text)
    , bpBuildToolOverrides :: Map Text (Set Text)
    }
    deriving (Show, Eq)

instance ToJSON BuildPlan where
    toJSON BuildPlan {..} = object
        [ "system-info" .= bpSystemInfo
        , "tools" .= fmap goTool bpTools
        , "packages" .= Map.mapKeysWith const unPackageName bpPackages
        , "github-users" .= bpGithubUsers
        , "build-tool-overrides" .= bpBuildToolOverrides
        ]
      where
        goTool (k, v) = object
            [ "name" .= display k
            , "version" .= display v
            ]
instance FromJSON BuildPlan where
    parseJSON = withObject "BuildPlan" $ \o -> do
        bpSystemInfo <- o .: "system-info"
        bpTools <- (o .: "tools") >>= T.mapM goTool
        bpPackages <- Map.mapKeysWith const mkPackageName <$> (o .: "packages")
        bpGithubUsers <- o .:? "github-users" .!= mempty
        bpBuildToolOverrides <- o .:? "build-tool-overrides" .!= mempty
        return BuildPlan {..}
      where
        goTool = withObject "Tool" $ \o -> (,)
            <$> ((o .: "name") >>=
                either (fail . show) return . simpleParse . asText)
            <*> ((o .: "version") >>=
                either (fail . show) return . simpleParse . asText)

data PackagePlan = PackagePlan
    { ppVersion     :: Version
    , ppCabalFileInfo :: Maybe CabalFileInfo
    , ppGithubPings :: Set Text
    , ppUsers       :: Set PackageName
    , ppConstraints :: PackageConstraints
    , ppDesc        :: SimpleDesc
    }
    deriving (Show, Eq)

instance ToJSON PackagePlan where
    toJSON PackagePlan {..} = object
        $ maybe id (\cfi -> (("cabal-file-info" .= cfi):)) ppCabalFileInfo $
        [ "version"      .= asText (display ppVersion)
        , "github-pings" .= ppGithubPings
        , "users"        .= Set.map unPackageName ppUsers
        , "constraints"  .= ppConstraints
        , "description"  .= ppDesc
        ]
instance FromJSON PackagePlan where
    parseJSON = withObject "PackageBuild" $ \o -> do
        ppVersion <- o .: "version"
                 >>= either (fail . show) return
                   . simpleParse . asText
        ppCabalFileInfo <- o .:? "cabal-file-info"
        ppGithubPings <- o .:? "github-pings" .!= mempty
        ppUsers <- Set.map PackageName <$> (o .:? "users" .!= mempty)
        ppConstraints <- o .: "constraints"
        ppDesc <- o .: "description"
        return PackagePlan {..}

-- | Information on the contents of a cabal file
data CabalFileInfo = CabalFileInfo
    { cfiSize :: !Int
    -- ^ File size in bytes
    , cfiHashes :: !(Map.Map Text Text)
    -- ^ Various hashes of the file contents
    }
    deriving (Show, Eq, Generic)
instance Bin.Binary CabalFileInfo
instance Bin.HasStructuralInfo CabalFileInfo
instance Bin.HasSemanticVersion CabalFileInfo
instance ToJSON CabalFileInfo where
    toJSON CabalFileInfo {..} = object
        [ "size" .= cfiSize
        , "hashes" .= cfiHashes
        ]
instance FromJSON CabalFileInfo where
    parseJSON = withObject "CabalFileInfo" $ \o -> do
        cfiSize <- o .: "size"
        cfiHashes <- o .: "hashes"
        return CabalFileInfo {..}

display :: DT.Text a => a -> Text
display = fromString . DT.display

simpleParse :: (MonadThrow m, DT.Text a, Typeable a) => Text -> m a
simpleParse orig = withTypeRep $ \rep ->
    case DT.simpleParse str of
        Nothing -> throwM (ParseFailedException rep (pack str))
        Just v  -> return v
  where
    str = unpack orig

    withTypeRep :: Typeable a => (TypeRep -> m a) -> m a
    withTypeRep f =
        res
      where
        res = f (typeOf (unwrap res))

        unwrap :: m a -> a
        unwrap _ = error "unwrap"

data ParseFailedException = ParseFailedException TypeRep Text
    deriving (Show, Typeable)
instance Exception ParseFailedException

unPackageName :: PackageName -> Text
unPackageName (PackageName str) = pack str

mkPackageName :: Text -> PackageName
mkPackageName = PackageName . unpack

data PackageConstraints = PackageConstraints
    { pcVersionRange     :: VersionRange
    , pcMaintainer       :: Maybe Maintainer
    , pcTests            :: TestState
    , pcBenches          :: TestState
    , pcHaddocks         :: TestState
    , pcFlagOverrides    :: Map FlagName Bool
    , pcEnableLibProfile :: Bool
    , pcSkipBuild        :: Bool
    -- ^ Don't even bother building this library, useful when dealing with
    -- OS-specific packages. See:
    -- https://github.com/fpco/stackage-curator/issues/3
    }
    deriving (Show, Eq)
instance ToJSON PackageConstraints where
    toJSON PackageConstraints {..} = object $ addMaintainer
        [ "version-range" .= display pcVersionRange
        , "tests" .= pcTests
        , "benches" .= pcBenches

        -- for backwards compatibility
        , "build-benchmarks" .=
            case pcBenches of
              Don'tBuild -> False
              _          -> True

        , "haddocks" .= pcHaddocks
        , "flags" .= Map.mapKeysWith const unFlagName pcFlagOverrides
        , "library-profiling" .= pcEnableLibProfile
        , "skip-build" .= pcSkipBuild
        ]
      where
        addMaintainer = maybe id (\m -> (("maintainer" .= m):)) pcMaintainer
instance FromJSON PackageConstraints where
    parseJSON = withObject "PackageConstraints" $ \o -> do
        pcVersionRange <- (o .: "version-range")
                      >>= either (fail . show) return . simpleParse
        pcTests <- o .: "tests"
        pcBenches <- o .: "benches" <|>
          -- Compatibility with old build-benchmarks boolean
          ((\x -> if x then ExpectFailure else Don'tBuild)
             <$> (o .: "build-benchmarks"))
        pcHaddocks <- o .: "haddocks"
        pcFlagOverrides <- Map.mapKeysWith const mkFlagName <$> o .: "flags"
        pcMaintainer <- o .:? "maintainer"
        pcEnableLibProfile <- fmap (fromMaybe True) (o .:? "library-profiling")
        pcSkipBuild <- o .:? "skip-build" .!= False
        return PackageConstraints {..}

data TestState = ExpectSuccess
               | ExpectFailure
               | Don'tBuild -- ^ when the test suite will pull in things we don't want
    deriving (Show, Eq, Ord, Bounded, Enum)

testStateToText :: TestState -> Text
testStateToText ExpectSuccess = "expect-success"
testStateToText ExpectFailure = "expect-failure"
testStateToText Don'tBuild    = "do-not-build"

instance ToJSON TestState where
    toJSON = toJSON . testStateToText
instance FromJSON TestState where
    parseJSON = withText "TestState" $ \t ->
        case HashMap.lookup t states of
            Nothing -> fail $ "Invalid state: " ++ unpack t
            Just v -> return v
      where
        states = HashMap.fromList
               $ map (\x -> (testStateToText x, x)) [minBound..maxBound]

data SystemInfo = SystemInfo
    { siGhcVersion      :: Version
    , siOS              :: OS
    , siArch            :: Arch
    , siCorePackages    :: Map PackageName Version
    , siCoreExecutables :: Set ExeName
    }
    deriving (Show, Eq, Ord)
instance ToJSON SystemInfo where
    toJSON SystemInfo {..} = object
        [ "ghc-version" .= display siGhcVersion
        , "os" .= display siOS
        , "arch" .= display siArch
        , "core-packages" .= Map.mapKeysWith const unPackageName (fmap display siCorePackages)
        , "core-executables" .= siCoreExecutables
        ]
instance FromJSON SystemInfo where
    parseJSON = withObject "SystemInfo" $ \o -> do
        let helper name = (o .: name) >>= either (fail . show) return . simpleParse
        siGhcVersion <- helper "ghc-version"
        siOS <- helper "os"
        siArch <- helper "arch"
        siCorePackages <- (o .: "core-packages") >>= goPackages
        siCoreExecutables <- o .: "core-executables"
        return SystemInfo {..}
      where
        goPackages = either (fail . show) return
                   . T.mapM simpleParse
                   . Map.mapKeysWith const mkPackageName

unFlagName :: FlagName -> Text
unFlagName (FlagName str) = pack str

mkFlagName :: Text -> FlagName
mkFlagName = FlagName . unpack

newtype Maintainer = Maintainer { unMaintainer :: Text }
    deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON, IsString)

-- | Name of an executable.
newtype ExeName = ExeName { unExeName :: Text }
    deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON, IsString)

-- | A simplified package description that tracks:
--
-- * Package dependencies
--
-- * Build tool dependencies
--
-- * Provided executables
--
-- It has fully resolved all conditionals
data SimpleDesc = SimpleDesc
    { sdPackages     :: Map PackageName DepInfo
    , sdTools        :: Map ExeName DepInfo
    , sdProvidedExes :: Set ExeName
    , sdModules      :: Set Text
    -- ^ modules exported by the library
    }
    deriving (Show, Eq)
instance Monoid SimpleDesc where
    mempty = SimpleDesc mempty mempty mempty mempty
    mappend (SimpleDesc a b c d) (SimpleDesc w x y z) = SimpleDesc
        (Map.unionWith (<>) a w)
        (Map.unionWith (<>) b x)
        (c <> y)
        (d <> z)
instance ToJSON SimpleDesc where
    toJSON SimpleDesc {..} = object
        [ "packages" .= Map.mapKeysWith const unPackageName sdPackages
        , "tools" .= Map.mapKeysWith const unExeName sdTools
        , "provided-exes" .= sdProvidedExes
        , "modules" .= sdModules
        ]
instance FromJSON SimpleDesc where
    parseJSON = withObject "SimpleDesc" $ \o -> do
        sdPackages <- Map.mapKeysWith const mkPackageName <$> (o .: "packages")
        sdTools <- Map.mapKeysWith const ExeName <$> (o .: "tools")
        sdProvidedExes <- o .: "provided-exes"
        sdModules <- o .: "modules"
        return SimpleDesc {..}

data DepInfo = DepInfo
    { diComponents :: Set Component
    , diRange      :: VersionRange
    }
    deriving (Show, Eq)

instance Semigroup DepInfo where
    DepInfo a x <> DepInfo b y = DepInfo
        (a <> b)
        (intersectVersionRanges x y)
instance ToJSON DepInfo where
    toJSON DepInfo {..} = object
        [ "components" .= diComponents
        , "range" .= display diRange
        ]
instance FromJSON DepInfo where
    parseJSON = withObject "DepInfo" $ \o -> do
        diComponents <- o .: "components"
        diRange <- o .: "range" >>= either (fail . show) return . simpleParse
        return DepInfo {..}

data Component = CompLibrary
               | CompExecutable
               | CompTestSuite
               | CompBenchmark
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

compToText :: Component -> Text
compToText CompLibrary = "library"
compToText CompExecutable = "executable"
compToText CompTestSuite = "test-suite"
compToText CompBenchmark = "benchmark"

instance ToJSON Component where
    toJSON = toJSON . compToText
instance FromJSON Component where
    parseJSON = withText "Component" $ \t -> maybe
        (fail $ "Invalid component: " ++ unpack t)
        return
        (HashMap.lookup t comps)
      where
        comps = HashMap.fromList $ map (compToText &&& id) [minBound..maxBound]

intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges x y = C.simplifyVersionRange $ C.intersectVersionRanges x y
