{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Stackage.Curator.RevDeps
    ( listRevDeps
    ) where

import Stackage.Prelude
import Data.Yaml (decodeFileEither)
import Control.Monad.State.Strict (execState, get, put)

listRevDeps :: FilePath
            -> Bool -- ^ deep revdeps
            -> PackageName -- ^ package to check
            -> IO ()
listRevDeps planFile deep pkg0 = do
    BuildPlan {..} <- decodeFileEither planFile >>= either throwIO return
    let go pkg = do
          visited <- get
          unless (pkg `member` visited) $ do
            put $ insertSet pkg visited
            case lookup pkg bpPackages of
              Nothing -> return ()
              Just PackagePlan {..} -> mapM_ go ppUsers
    let pkgs = execState (go pkg0) (asSet mempty)
    mapM_ (putStrLn . pack . unPackageName) pkgs
