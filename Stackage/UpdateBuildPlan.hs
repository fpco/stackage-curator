{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Take an existing build plan and bump all packages to the newest version in
-- the same major version number.
module Stackage.UpdateBuildPlan
    ( updateBuildConstraints
    , updateBuildPlan
    ) where

import qualified Data.Map                  as Map
import           Distribution.Version      (anyVersion, earlierVersion,
                                            orLaterVersion)
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.PackageIndex
import           Stackage.Prelude

updateBuildPlan :: Map PackageName PackagePlan -> BuildPlan -> IO BuildPlan
updateBuildPlan packagesOrig bp = do
    allCabalHashesCommit <- getAllCabalHashesCommit
    -- mempty, since when updating a build plan, we don't deal with tell-me-when-its-released
    newBuildPlan allCabalHashesCommit packagesOrig mempty $ updateBuildConstraints bp

updateBuildConstraints :: BuildPlan -> BuildConstraints
updateBuildConstraints BuildPlan {..} =
    BuildConstraints {..}
  where
    bcSystemInfo = bpSystemInfo
    bcPackages = Map.keysSet bpPackages
    bcGithubUsers = bpGithubUsers
    bcBuildToolOverrides = bpBuildToolOverrides
    bcTellMeWhenItsReleased = mempty -- we don't care when doing an update
    bcNoRevisions = bpNoRevisions

    bcPackageConstraints name = PackageConstraints
        { pcVersionRange = addBumpRange (maybe anyVersion pcVersionRange moldPC)
        , pcConfigureArgs = maybe mempty pcConfigureArgs moldPC
        , pcMaintainer = moldPC >>= pcMaintainer
        , pcTests = maybe ExpectSuccess pcTests moldPC
        , pcHaddocks = maybe ExpectSuccess pcHaddocks moldPC
        , pcBenches = maybe ExpectSuccess pcBenches moldPC
        , pcFlagOverrides = maybe mempty pcFlagOverrides moldPC
        , pcEnableLibProfile = maybe True pcEnableLibProfile moldPC
        , pcSkipBuild = maybe False pcSkipBuild moldPC
        , pcHide = maybe False pcHide moldPC
        }
      where
        moldBP = lookup name bpPackages
        moldPC = ppConstraints <$> moldBP

        addBumpRange oldRange =
            case moldBP of
                Nothing -> oldRange
                Just bp -> intersectVersionRanges oldRange
                         $ bumpRange $ ppVersion bp

    bumpRange version = intersectVersionRanges
        (orLaterVersion version)
        (earlierVersion $ bumpVersion $ versionNumbers version)
    bumpVersion (x:y:_) = mkVersion [x, y + 1]
    bumpVersion [x] = mkVersion [x, 1]
    bumpVersion [] = assert False $ mkVersion [1, 0]
