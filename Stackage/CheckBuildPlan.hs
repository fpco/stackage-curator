{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies               #-}
-- | Confirm that a build plan has a consistent set of dependencies.
module Stackage.CheckBuildPlan
    ( checkBuildPlan
    , BadBuildPlan
    ) where

import           Control.Monad.Writer.Strict (Writer, execWriter, tell)
import qualified Data.Map.Strict as M
import           Data.Semigroup (Option (..), Max (..))
import qualified Data.Text as T
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.PackageDescription
import           Stackage.Prelude

-- | Check the build plan for missing deps, wrong versions, etc.
checkBuildPlan :: (MonadThrow m)
               => Bool -- ^ fail on missing Cabal package
               -> BuildPlan
               -> m ()
checkBuildPlan failMissingCabal BuildPlan {..}
    | null errs1 && null errs2 = return ()
    | otherwise = throwM errs
  where
    allPackages = map (,mempty) (siCorePackages bpSystemInfo) ++
                  map (ppVersion &&& M.keys . M.filter libAndExe . sdPackages . ppDesc) bpPackages
    errs@(BadBuildPlan errs1 errs2) = execWriter $ do
        mapM_ (checkDeps getMaint allPackages) $ mapToList bpPackages
        let cabalName = PackageName "Cabal"
        case lookup cabalName bpPackages of
            Nothing
                | failMissingCabal -> tell
                    $ BadBuildPlan mempty
                    $ singletonMap cabalName
                    $ singleton "Cabal not found in build plan"
                | otherwise -> return ()
            Just (ppVersion -> cabalVersion) ->
                mapM_ (checkCabalVersion cabalVersion) (mapToList bpPackages)
    -- Only looking at libraries and executables, benchmarks and tests
    -- are allowed to create cycles (e.g. test-framework depends on
    -- text, which uses test-framework in its test-suite).
    libAndExe (DepInfo cs _) = any (flip elem [CompLibrary,CompExecutable]) cs

    getMaint :: PackageName -> Maybe Maintainer
    getMaint pn = do
        pp <- lookup pn bpPackages
        pcMaintainer $ ppConstraints pp

-- | For a given package name and plan, check that its dependencies are:
--
-- 1. Existent (existing in the provided package map)
-- 2. Within version range
-- 3. Check for dependency cycles.
checkDeps :: (PackageName -> Maybe Maintainer)
          -> Map PackageName (Version,[PackageName])
          -> (PackageName, PackagePlan)
          -> Writer BadBuildPlan ()
checkDeps getMaint allPackages (user, pb) =
    mapM_ go $ mapToList $ sdPackages $ ppDesc pb
  where
    go (dep, diRange -> range) =
        case lookup dep allPackages of
            Nothing -> tell $ BadBuildPlan (singletonMap (dep, getMaint dep, Nothing) errMap) mempty
            Just (version,deps)
                | version `withinRange` range ->
                    occursCheck allPackages
                                (\d v ->
                                     tell $ BadBuildPlan (singletonMap
                                     (d, getMaint dep, v)
                                     errMap) mempty)
                                dep
                                deps
                                []
                | otherwise -> tell $ BadBuildPlan (singletonMap
                    (dep, getMaint dep, Just version)
                    errMap) mempty
      where
        errMap = singletonMap pu range
        pu = PkgUser
            { puName = user
            , puVersion = ppVersion pb
            , puMaintainer = pcMaintainer $ ppConstraints pb
            , puGithubPings = ppGithubPings pb
            }

-- | Ensure our selected Cabal version is sufficient for the given
-- package
checkCabalVersion :: Version -> (PackageName, PackagePlan) -> Writer BadBuildPlan ()
checkCabalVersion cabalVersion (name, plan)
  | Option (Just (Max neededVersion)) <- sdCabalVersion (ppDesc plan) =
    unless (cabalVersion >= neededVersion) $ tell $ BadBuildPlan
           mempty $ singletonMap name $ singleton $ concat
                  [ "Cabal version "
                  , display cabalVersion
                  , " sufficient for "
                  , display neededVersion
                  ]
  | otherwise = return ()

-- | Check whether the package(s) occurs within its own dependency
-- tree.
occursCheck
    :: Monad m
    => Map PackageName (Version,[PackageName])
    -- ^ All packages.
    -> (PackageName -> Maybe Version -> m ())
    -- ^ Report an erroneous package.
    -> PackageName
    -- ^ Starting package to check for cycles in.
    -> [PackageName]
    -- ^ Dependencies of the package.
    -> [PackageName]
    -- ^ Previously seen packages up the dependency tree.
    -> m ()
occursCheck allPackages reportError =
    go
    where
        go pkg deps seen =
            case find (flip elem seen) deps of
                Just cyclic ->
                    reportError cyclic $
                    fmap fst (lookup cyclic allPackages)
                Nothing ->
                    forM_ deps $
                    \pkg' ->
                         case lookup pkg' allPackages of
                             Just (_v,deps')
                                 | pkg' /= pkg -> go pkg' deps' seen'
                             _ -> return ()
            where seen' = pkg : seen

data PkgUser = PkgUser
    { puName        :: PackageName
    , puVersion     :: Version
    , puMaintainer  :: Maybe Maintainer
    , puGithubPings :: Set Text
    }
    deriving (Eq, Ord)

pkgUserShow1 :: PkgUser -> Text
pkgUserShow1 PkgUser {..} = concat
    [ display puName
    , "-"
    , display puVersion
    ]

pkgUserShow2 :: PkgUser -> Text
pkgUserShow2 PkgUser {..} = unwords
    $ (maybe "No maintainer" unMaintainer puMaintainer ++ ".")
    : map (cons '@') (setToList puGithubPings)

data BadBuildPlan = BadBuildPlan
     (Map (PackageName, Maybe Maintainer, Maybe Version) (Map PkgUser VersionRange))
     (Map PackageName (Vector Text))
    deriving Typeable
instance Exception BadBuildPlan
instance Show BadBuildPlan where
    show (BadBuildPlan errs1 errs2) =
        unpack $ concatMap go1 (mapToList errs1) ++ concatMap go2 (mapToList errs2)
      where
        go1 ((dep, mmaint, mdepVer), users) = unlines
            $ ""
            : showDepVer dep mmaint mdepVer
            : map showUser (mapToList users)

        showDepVer :: PackageName
                   -> Maybe Maintainer
                   -> Maybe Version
                   -> Text
        showDepVer dep mmaint Nothing = T.concat
            [ display dep
            , displayMaint mmaint
            , " (not present) depended on by:"
            ]
        showDepVer dep mmaint (Just version) = concat
            [ display dep
            , "-"
            , display version
            , displayMaint mmaint
            , " is out of bounds for:"
            ]

        displayMaint Nothing = ""
        displayMaint (Just (Maintainer t)) = T.concat
            [ " ("
            , t
            , ")"
            ]

        showUser :: (PkgUser, VersionRange) -> Text
        showUser (pu, range) = concat
            [ "- [ ] "
            , pkgUserShow1 pu
            , " ("
            -- add a space after < to avoid confusing Markdown processors (like
            -- Github's issue tracker)
            , T.replace "<" "< " $ display range
            , "). "
            , pkgUserShow2 pu
            ]

        go2 :: (PackageName, Vector Text) -> Text
        go2 (name, errs) = unlines
          $ display name
          : map (\err -> "    " ++ err) (toList errs)

instance Monoid BadBuildPlan where
    mempty = BadBuildPlan mempty mempty
    mappend (BadBuildPlan a x) (BadBuildPlan b y) = BadBuildPlan
        (unionWith (unionWith intersectVersionRanges) a b)
        (unionWith mappend x y)
