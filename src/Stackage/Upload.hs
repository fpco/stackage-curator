-- | Upload to Stackage and Hackage
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE FlexibleContexts           #-}
module Stackage.Upload (uploadHackageDistro) where

import Network.HTTP.Client
import Stackage.Prelude
import Stackage.ServerBundle                 (bpAllPackages)

uploadHackageDistro
    :: Text -- ^ distro name
    -> BuildPlan
    -> ByteString -- ^ Hackage username
    -> ByteString -- ^ Hackage password
    -> Manager
    -> IO (Response LByteString)
uploadHackageDistro name bp username password manager = do
    req1 <- parseRequest $ concat
        [ "https://hackage.haskell.org/distro/"
        , unpack name
        , "/packages.csv"
        ]
    let req2 = req1
                { requestHeaders = [("Content-Type", "text/csv")]
                , requestBody = RequestBodyLBS csv
                , method = "PUT"
                }
    httpLbs (applyBasicAuth username password req2) manager
  where
    csv = encodeUtf8
        $ builderToLazy
        $ mconcat
        $ intersperse "\n"
        $ map go
        $ mapToList
        $ bpAllPackages bp
    go (name', version) =
        "\"" ++
        (toBuilder $ display name') ++
        "\",\"" ++
        (toBuilder $ display version) ++
        "\",\"https://www.stackage.org/package/" ++
        (toBuilder $ display name') ++
        "\""
