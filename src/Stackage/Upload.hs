-- | Upload to Stackage and Hackage
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE FlexibleContexts           #-}
module Stackage.Upload
    ( uploadHackageDistro
    , uploadBundleV2
    , UploadBundleV2 (..)
    , def
    , StackageServer
    , unStackageServer
    ) where

import Data.Default.Class                    (Default (..))
import Data.Function                         (fix)
import Network.HTTP.Client
import qualified Network.HTTP.Client.Conduit as HCC
import Stackage.Prelude
import Stackage.ServerBundle                 (bpAllPackages)
import qualified System.IO as IO

newtype StackageServer = StackageServer { unStackageServer :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString)
instance Default StackageServer where
    def = "https://www.stackage.org"

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

data UploadBundleV2 = UploadBundleV2
    { ub2Server :: StackageServer
    , ub2AuthToken :: Text
    , ub2Bundle :: FilePath
    }

uploadBundleV2 :: UploadBundleV2 -> Manager -> IO Text
uploadBundleV2 UploadBundleV2 {..} man = IO.withBinaryFile ub2Bundle IO.ReadMode $ \h -> do
    size <- IO.hFileSize h
    putStrLn $ "Bundle size: " ++ tshow size
    req1 <- parseUrlThrow $ unpack $ unStackageServer ub2Server ++ "/upload2"
    let req2 = req1
            { method = "PUT"
            , requestHeaders =
                [ ("Authorization", encodeUtf8 ub2AuthToken)
                , ("Accept", "application/json")
                , ("Content-Type", "application/x-tar")
                ]
            , requestBody = HCC.requestBodySource (fromIntegral size)
                          $ sourceHandle h .| printProgress size
            }
        sink = decodeUtf8C .| fix (\loop -> do
            mx <- peekC
            case mx of
                Nothing -> error $ "uploadBundleV2: premature end of stream"
                Just _ -> do
                    l <- lineC $ takeCE 4096 .| foldC
                    let (cmd, msg') = break (== ':') l
                        msg = dropWhile (== ' ') $ dropWhile (== ':') msg'
                    case cmd of
                        "CONT" -> do
                            putStrLn msg
                            loop
                        "FAILURE" -> error $ "uploadBundleV2 failed: " ++ unpack msg
                        "SUCCESS" -> return msg
                        _ -> error $ "uploadBundleV2: unknown command " ++ unpack cmd
            )
    withResponse req2 man $ \res -> runConduit $ HCC.bodyReaderSource (responseBody res) .| sink
  where
    printProgress total =
        loop 0 0
      where
        loop sent lastPercent =
            await >>= maybe (putStrLn "Upload complete") go
          where
            go bs = do
                yield bs
                let sent' = sent + fromIntegral (length bs)
                    percent = sent' * 100 `div` total
                when (percent /= lastPercent)
                    $ putStrLn $ "Upload progress: " ++ tshow percent ++ "%"
                loop sent' percent
