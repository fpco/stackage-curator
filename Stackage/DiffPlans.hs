{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Stackage.DiffPlans
    ( diffPlans
    ) where

import           Data.Text (Text, justifyLeft)
import           Data.Map (filterWithKey)
import qualified Data.Text as T
import           Data.Yaml (decodeFileEither)
import           Stackage.Prelude

data xChange = Added | Deleted | MajorBump | MinorBump | Unchanged
    deriving (Show, Eq, Ord)

data AndOr a = Old a | New a | Both a a
    deriving Show
instance Semigroup (AndOr a) where
    Old x    <> New y = Both x y
    New y    <> Old x = Both x y
    Old x    <> Old _ = Old x
    New x    <> New _ = New x
    Both x y <> _     = Both x y

diffPlans :: FilePath -- ^ old YAML build plan file
          -> FilePath -- ^ new YAML build plan file
          -> Bool     -- ^ show all or just changed packages
          -> Bool     -- ^ use colours
          -> IO ()
diffPlans oldFP newFP diffsOnly useColor = do
    old <- fmap Old <$> parse oldFP
    new <- fmap New <$> parse newFP
    let combined = unionWith (<>) old new
        m :: Map Change (Map PackageName Text)
        m = unionsWith mappend . map go $ mapToList combined
        f = if diffsOnly
               then filterWithKey (\a _ -> a /= Unchanged)
               else id

    forM_ (mapToList $ f m) $ \(change, m') -> do
        print change
        forM_ (mapToList m') $ \(pkg, msg) ->
            putStrLn $ justifyLeft 25 ' ' (display pkg <> ": ") <> msg
        putStrLn ""
  where
    parse fp = decodeFileEither (fpToString fp)
           >>= either throwIO (return . toSimple)

    toSimple = fmap ppVersion . bpPackages

    go (name, Old x) = singletonMap Deleted   . singletonMap name . red   $ display x
    go (name, New x) = singletonMap Added     . singletonMap name . green $ display x
    go (name, Both x y)
        | x == y     = singletonMap Unchanged . singletonMap name $         display x
        | otherwise  = singletonMap
                  (if isMajor x y then MajorBump else MinorBump)
                  (singletonMap name (concat
                   [ justifyLeft 9 ' ' $ display x
                   , " ==> "
                   , (if isMajor x y then yellow else blue) $ display y
                   ]))
    showInColor col s =
      if useColor
        then "\x1b[" <> col <> "m" <> s <> "\x1b[0m"
        else s
    black   = showInColor "30"
    red     = showInColor "31"
    green   = showInColor "32"
    yellow  = showInColor "33"
    blue    = showInColor "34"
    magenta = showInColor "35"
    cyan    = showInColor "36"
    white   = showInColor "37"

isMajor :: Version -> Version -> Bool
isMajor (Version old _) (Version new _) =
    toPair old /= toPair new
  where
    toPair []      = (0, 0)
    toPair [i]     = (i, 0)
    toPair (i:j:_) = (i, j)
