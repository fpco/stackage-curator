{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
module Stackage.Curator.UploadIndex
    ( uploadIndex
    ) where

import Data.Yaml                 (decodeFileEither)
import Stackage.BuildConstraints
import Stackage.BuildPlan
import Stackage.Prelude
import Stackage.PackageIndex
import qualified Codec.Archive.Tar as Tar
import Data.Conduit.Lazy (lazyConsume)
import Codec.Compression.GZip (compress)
import           Network.AWS                   (Credentials (Discover),
                                                Region (NorthVirginia), newEnv)
import Stackage.Curator.UploadDocs (upload)

uploadIndex
    :: FilePath -- ^ build plan
    -> Target
    -> Text -- ^ bucket
    -> Text -- ^ key prefix
    -> IO ()
uploadIndex bpFile target bucket prefix = do
    env <- newEnv NorthVirginia Discover
    bp <- decodeFileEither bpFile >>= either throwM return
    let toInclude = getToInclude bp
    runResourceT $ do
        entries <- lazyConsume
            $  sourcePackageIndex
            $= filterC toInclude
            $= mapC ucfEntry
        let lbs = compress $ Tar.write entries
            key = concat
                [ prefix
                , targetSlug target
                , ".tar.gz"
                ]
        sourceLazy lbs $$ upload False env bucket key

getToInclude :: BuildPlan -> UnparsedCabalFile -> Bool
getToInclude bp =
    go
  where
    go cfe = lookup (ucfName cfe) packages == Just (ucfVersion cfe)

    packages = siCorePackages (bpSystemInfo bp) ++
               (ppVersion <$> bpPackages bp)
