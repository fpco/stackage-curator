{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage.PackageIndexSpec (spec) where

import Stackage.PackageIndex
import Stackage.Prelude
import Test.Hspec
import System.Directory (doesFileExist, getAppUserDataDirectory)

spec :: Spec
spec = do
    it "works" $ ifIndexExists $ runConduitRes $ sourcePackageIndex .| sinkNull
    it "getLatestDescriptions gives reasonable results" $ ifIndexExists $ do
        let f x y = (display x, display y) `member` asSet (setFromList
                [ (asText "base", asText "4.5.0.0")
                , ("does-not-exist", "9999999999999999999")
                ])
        (m, _) <- getLatestDescriptions mempty f return
        length m `shouldBe` 1
        p <- simpleParse $ asText "base"
        v <- simpleParse $ asText "4.5.0.0"
        (spdVersion <$> m) `shouldBe` singletonMap p v

ifIndexExists :: IO () -> IO ()
ifIndexExists inner = do
    stack <- getAppUserDataDirectory "stack"
    let fp = stack </> "indices" </> "Hackage" </> "00-index.tar"
    exists <- doesFileExist fp
    if exists
        then inner
        else pendingWith "00-index.tar not available, skipping test"
