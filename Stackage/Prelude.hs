{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Stackage.Prelude
    ( module X
    , module Stackage.Prelude
    ) where

import           ClassyPrelude.Conduit           as X
import           Data.Conduit.Process            as X
import qualified Data.Map                        as Map
import           Distribution.Package            as X (PackageIdentifier (..), PackageName (PackageName))
import           Distribution.PackageDescription as X (FlagName (..), GenericPackageDescription)
import           Distribution.Version            as X (Version (..),
                                                       VersionRange)
import           Distribution.Version            as X (withinRange)
import qualified Distribution.Version            as C
import           Filesystem                      (createTree)
import           Filesystem.Path                 (parent)
import qualified Filesystem.Path.CurrentOS       as F
import Stackage.Types as X

-- | There seems to be a bug in Cabal where serializing and deserializing
-- version ranges winds up with different representations. So we have a
-- super-simplifier to deal with that.
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange vr =
    fromMaybe (assert False vr') $ simpleParse $ display vr'
  where
    vr' = C.simplifyVersionRange vr

-- | Topologically sort so that items with dependencies occur after those
-- dependencies.
topologicalSort :: (Ord key, Show key, MonadThrow m, Typeable key)
                => (value -> finalValue)
                -> (value -> Set key) -- ^ deps
                -> Map key value
                -> m (Vector (key, finalValue))
topologicalSort toFinal toDeps =
    loop id . mapWithKey removeSelfDeps . fmap (toDeps &&& toFinal)
  where
    removeSelfDeps k (deps, final) = (deleteSet k deps, final)
    loop front toProcess | null toProcess = return $ pack $ front []
    loop front toProcess
        | null noDeps = throwM $ NoEmptyDeps (map fst toProcess')
        | otherwise = loop (front . noDeps') (mapFromList hasDeps)
      where
        toProcess' = fmap (first removeUnavailable) toProcess
        allKeys = Map.keysSet toProcess
        removeUnavailable = asSet . setFromList . filter (`member` allKeys) . setToList
        (noDeps, hasDeps) = partition (null . fst . snd) $ mapToList toProcess'
        noDeps' = (map (second snd) noDeps ++)

data TopologicalSortException key = NoEmptyDeps (Map key (Set key))
    deriving (Show, Typeable)
instance (Show key, Typeable key) => Exception (TopologicalSortException key)

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dest =
    runResourceT $ sourceDirectoryDeep False src $$ mapM_C go
  where
    src' = fromString src F.</> ""
    go fp = forM_ (F.stripPrefix src' $ fromString fp) $ \suffix -> do
        let dest' = dest </> F.encodeString suffix
        liftIO $ createTree $ parent $ fromString dest'
        sourceFile fp $$ (sinkFile dest' :: Sink ByteString (ResourceT IO) ())

data Target = TargetNightly !Day
            | TargetLts !Int !Int
    deriving Show

targetSlug :: Target -> Text
targetSlug (TargetNightly day) = "nightly-" ++ tshow day
targetSlug (TargetLts x y) = concat ["lts-", tshow x, ".", tshow y]
