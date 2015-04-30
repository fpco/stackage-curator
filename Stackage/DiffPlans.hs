{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Stackage.DiffPlans
    ( diffPlans
    ) where

import Stackage.Prelude
import Data.Yaml (decodeFileEither)

data Change = Added | Deleted | MajorBump | MinorBump | Unchanged
    deriving (Show, Eq, Ord)

data AndOr a = Old a | New a | Both a a
    deriving Show
instance Semigroup (AndOr a) where
    Old x <> New y = Both x y
    New y <> Old x = Both x y
    Old x <> Old _ = Old x
    New x <> New _ = New x
    Both x y <> _ = Both x y

diffPlans :: FilePath -- ^ old YAML build plan file
          -> FilePath -- ^ new YAML build plan file
          -> IO ()
diffPlans oldFP newFP = do
    old <- fmap Old <$> parse oldFP
    new <- fmap New <$> parse newFP
    let combined = unionWith (<>) old new
        m :: Map Change (Map PackageName Text)
        m = unionsWith mappend $ map go $ mapToList combined

    forM_ (mapToList m) $ \(change, m') -> do
        print change
        forM_ (mapToList m') $ \(pkg, msg) -> do
            putStrLn $ concat
                [ display pkg
                , ": "
                , msg
                ]
        putStrLn ""
  where
    parse fp = decodeFileEither (fpToString fp)
           >>= either throwIO (return . toSimple)

    toSimple = fmap ppVersion . bpPackages

    go (name, Old x) = singletonMap Deleted $ singletonMap name $ display x
    go (name, New x) = singletonMap Added $ singletonMap name $ display x
    go (name, Both x y)
        | x == y = singletonMap Unchanged $ singletonMap name $ display x
        | otherwise = singletonMap
            (if isMajor x y then MajorBump else MinorBump)
            (singletonMap name (concat
                [ display x
                , " ==> "
                , display y
                ]))

isMajor :: Version -> Version -> Bool
isMajor (Version old _) (Version new _) =
    toPair old /= toPair new
  where
    toPair [] = (0, 0)
    toPair [i] = (i, 0)
    toPair (i:j:_) = (i, j)
