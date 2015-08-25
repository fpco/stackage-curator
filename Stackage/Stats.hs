{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage.Stats
    ( printStats
    ) where

import Stackage.Prelude
import Data.Yaml (decodeFileEither)

printStats :: FilePath -- ^ YAML build plan file
           -> IO ()
printStats fp = do
    bp <- decodeFileEither fp >>= either throwIO return
    let core = length $ siCorePackages $ bpSystemInfo bp
        pkgs = length $ bpPackages bp
        maintainers = length $ asSet $ flip foldMap (bpPackages bp)
            $ maybe
                mempty
                singletonSet
                . pcMaintainer . ppConstraints
    putStrLn $ "Core packages:      " ++ tshow core
    putStrLn $ "Non-core packages:  " ++ tshow pkgs
    putStrLn $ "Total packages:     " ++ tshow (core + pkgs)
    putStrLn $ "Unique maintainers: " ++ tshow maintainers
