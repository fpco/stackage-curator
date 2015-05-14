{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
module Stackage.Curator.UploadIndex
    ( uploadIndex
    ) where

import System.Directory (getAppUserDataDirectory)
import Distribution.Package (Dependency)
import Filesystem (isDirectory, createTree, isFile, rename)
import Filesystem.Path (parent)
import Control.Concurrent        (threadDelay, getNumCapabilities)
import Control.Concurrent.Async  (withAsync)
import Data.Default.Class        (def)
import Data.Semigroup            (Max (..), Option (..))
import Data.Text.Read            (decimal)
import Data.Time
import Data.Yaml                 (decodeFileEither, encodeFile, decodeEither')
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Stackage.BuildConstraints
import Stackage.BuildPlan
import Stackage.CheckBuildPlan
import Stackage.PerformBuild
import Stackage.Prelude
import Stackage.ServerBundle
import Stackage.UpdateBuildPlan
import Stackage.Upload
import System.Environment        (lookupEnv)
import System.IO                 (BufferMode (LineBuffering), hSetBuffering)
import Control.Monad.Trans.Unlift (askRunBase, MonadBaseUnlift)
import Data.Function (fix)
import Control.Concurrent.Async (Concurrently (..))
import Stackage.Curator.UploadDocs (uploadDocs)
import Stackage.Install (defaultIndexLocation)
import Stackage.PackageIndex.Conduit
import qualified Codec.Archive.Tar as Tar
import Data.Conduit.Lazy (lazyConsume)
import Codec.Compression.GZip (compress)
import           Network.AWS                   (Credentials (Discover), Env,
                                                Region (NorthVirginia), getEnv,
                                                send)
import           Network.AWS.Data              (toBody)
import           Network.AWS.S3                (ObjectCannedACL (PublicRead),
                                                poACL, poCacheControl,
                                                poContentEncoding,
                                                poContentType, putObject)
import Stackage.Curator.UploadDocs (upload)

uploadIndex
    :: FilePath -- ^ build plan
    -> Target
    -> Text -- ^ bucket
    -> Text -- ^ key prefix
    -> IO ()
uploadIndex bpFile target bucket prefix = do
    env <- getEnv NorthVirginia Discover
    bp <- decodeFileEither (fpToString bpFile) >>= either throwM return
    let toInclude = getToInclude bp
    runResourceT $ do
        entries <- lazyConsume
            $  sourceAllCabalFiles defaultIndexLocation
            $= filterC toInclude
            $= mapC cfeEntry
        let lbs = compress $ Tar.write entries
            key = concat
                [ prefix
                , targetSlug target
                , ".tar.gz"
                ]
        sourceLazy lbs $$ upload False env bucket key

getToInclude :: BuildPlan -> CabalFileEntry -> Bool
getToInclude bp =
    go
  where
    go cfe = lookup (cfeName cfe) packages == Just (cfeVersion cfe)

    packages = siCorePackages (bpSystemInfo bp) ++
               (ppVersion <$> bpPackages bp)
