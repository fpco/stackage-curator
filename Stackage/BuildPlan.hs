{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleContexts   #-}
-- | Representation of a concrete build plan, and how to generate a new one
-- based on constraints.
module Stackage.BuildPlan
    ( BuildPlan (..)
    , PackagePlan (..)
    , newBuildPlan
    , makeToolMap
    , getLatestAllowedPlans
    ) where

import           Control.Monad.State.Strict      (execState, get, put)
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Distribution.Compiler
import           Distribution.PackageDescription
import           Stackage.BuildConstraints
import           Stackage.GithubPings
import           Stackage.PackageDescription
import           Stackage.PackageIndex
import           Stackage.Prelude

-- | Make a build plan given these package set and build constraints.
newBuildPlan :: MonadIO m => Map PackageName PackagePlan -> BuildConstraints -> m BuildPlan
newBuildPlan packagesOrig bc@BuildConstraints {..} = liftIO $ do
    let toolMap :: Map ExeName (Set PackageName)
        toolMap = makeToolMap bcBuildToolOverrides packagesOrig
        packages = populateUsers $ removeUnincluded bc toolMap packagesOrig
        toolNames :: [ExeName]
        toolNames = concatMap (Map.keys . sdTools . ppDesc) packages
    tools <- topologicalSortTools toolMap $ mapFromList $ do
        exeName <- toolNames
        guard $ exeName `notMember` siCoreExecutables
        packageName <- maybe mempty setToList $ lookup exeName toolMap
        packagePlan <- maybeToList $ lookup packageName packagesOrig
        return (packageName, packagePlan)
    -- FIXME topologically sort packages? maybe just leave that to the build phase
    return BuildPlan
        { bpSystemInfo = bcSystemInfo
        , bpTools = tools
        , bpPackages = packages
        , bpGithubUsers = bcGithubUsers
        , bpBuildToolOverrides = bcBuildToolOverrides
        }
  where
    SystemInfo {..} = bcSystemInfo

makeToolMap :: Map Text (Set Text) -- ^ build tool overrides
            -> Map PackageName PackagePlan
            -> Map ExeName (Set PackageName)
makeToolMap overrides =
    (overrides' ++) . unionsWith (++) . map go . mapToList
  where
    go (packageName, pp) =
        foldMap go' $ sdProvidedExes $ ppDesc pp
      where
        go' exeName = singletonMap exeName (singletonSet packageName)

    overrides' :: Map ExeName (Set PackageName)
    overrides' = Map.mapKeysWith (++) ExeName
               $ fmap (Set.map mkPackageName) overrides

topologicalSortTools :: MonadThrow m
                     => Map ExeName (Set PackageName)
                     -> Map PackageName PackagePlan
                     -> m (Vector (PackageName, Version))
topologicalSortTools toolMap = topologicalSort
    ppVersion
    (concatMap (fromMaybe mempty . flip lookup toolMap) . Map.keys . sdTools . ppDesc)

-- | Include only packages which are dependencies of the required packages and
-- their build tools.
removeUnincluded :: BuildConstraints
                 -> Map ExeName (Set PackageName)
                 -> Map PackageName PackagePlan
                 -> Map PackageName PackagePlan
removeUnincluded BuildConstraints {..} toolMap orig =
    mapFromList $ filter (\(x, _) -> x `member` included) $ mapToList orig
  where
    SystemInfo {..} = bcSystemInfo

    included :: Set PackageName
    included = flip execState mempty $ mapM_ add bcPackages

    add name = do
        inc <- get
        when (name `notMember` inc) $ do
            put $ insertSet name inc
            case lookup name orig of
                Nothing -> return ()
                Just pb -> do
                    mapM_ add $ Map.keys $ sdPackages $ ppDesc pb
                    forM_ (Map.keys $ sdTools $ ppDesc pb) $
                        \exeName -> when (exeName `notMember` siCoreExecutables)
                            $ mapM_ add $ fromMaybe mempty $ lookup exeName toolMap

populateUsers :: Map PackageName PackagePlan
              -> Map PackageName PackagePlan
populateUsers orig =
    mapWithKey go orig
  where
    go name pb = pb { ppUsers = foldMap (go2 name) (mapToList orig) }

    go2 dep (user, pb)
        | dep `member` sdPackages (ppDesc pb) = singletonSet user
        | otherwise = mempty

-- | Check whether the given package/version combo meets the constraints
-- currently in place.
isAllowed :: BuildConstraints
          -> PackageName -> Version -> Bool
isAllowed bc = \name version ->
    case lookup name $ siCorePackages $ bcSystemInfo bc of
        Just _ -> False -- never reinstall a core package
        Nothing -> withinRange version $ pcVersionRange $ bcPackageConstraints bc name

mkPackagePlan :: MonadThrow m
              => BuildConstraints
              -> SimplifiedPackageDescription
              -> m PackagePlan
mkPackagePlan bc spd = do
    ppDesc <- toSimpleDesc CheckCond {..} spd
    return PackagePlan {..}
  where
    name = spdName spd
    ppVersion = spdVersion spd
    ppCabalFileInfo = Just $ spdCabalFileInfo spd
    ppGithubPings = applyGithubMapping bc $ spdGithubPings spd
    ppConstraints = onlyRelevantFlags $ bcPackageConstraints bc name
    ppUsers = mempty -- must be filled in later

    -- Only include flags that are actually provided by the package. For more
    -- information, see: https://github.com/fpco/stackage-curator/issues/11
    onlyRelevantFlags :: PackageConstraints -> PackageConstraints
    onlyRelevantFlags pc = pc
        { pcFlagOverrides = pcFlagOverrides pc `intersection`
                            spdPackageFlags spd
        }

    ccPackageName = name
    ccOS = siOS
    ccArch = siArch
    ccCompilerFlavor = Distribution.Compiler.GHC
    ccCompilerVersion = siGhcVersion
    ccFlags = flags
    ccIncludeTests = pcTests ppConstraints /= Don'tBuild
    ccIncludeBenchmarks = pcBenches ppConstraints /= Don'tBuild

    SystemInfo {..} = bcSystemInfo bc

    overrides = pcFlagOverrides ppConstraints
    flags = mapWithKey overrideFlag $ spdPackageFlags spd
    overrideFlag name defVal = fromMaybe defVal $ lookup name overrides

getLatestAllowedPlans :: MonadIO m => BuildConstraints -> m (Map PackageName PackagePlan)
getLatestAllowedPlans bc =
    getLatestDescriptions
        (isAllowed bc)
        (mkPackagePlan bc)
