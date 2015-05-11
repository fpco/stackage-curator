{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Upload Haddock documentation to S3.
module Stackage.Curator.UploadDocs
    ( uploadDocs
    ) where
import           ClassyPrelude.Conduit
import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Archive.Tar.Entry       as Tar
import           Control.Monad.State.Strict    (MonadState, evalStateT, get,
                                                modify)
import           Control.Monad.Trans.Resource  (liftResourceT)
import           Control.Monad.Writer.Strict   (MonadWriter, execWriterT, tell)
import           Crypto.Hash                   (Digest, SHA256)
import           Crypto.Hash.Conduit           (sinkHash)
import           Data.Byteable                 (toBytes)
import qualified Data.ByteString.Base16        as B16
import           Data.Conduit.Zlib             (WindowBits (WindowBits),
                                                compress)
import           Data.XML.Types                (Content (ContentText), Event (EventBeginDoctype, EventEndDoctype, EventBeginElement),
                                                Name)
import qualified Filesystem                    as F
import qualified Filesystem.Path.CurrentOS     as F
import           Network.AWS                   (Credentials (Discover), Env,
                                                Region (NorthVirginia), getEnv,
                                                send)
import           Network.AWS.Data              (toBody)
import           Network.AWS.S3                (ObjectCannedACL (PublicRead),
                                                poACL, poCacheControl,
                                                poContentEncoding,
                                                poContentType, putObject)
import           Network.Mime                  (defaultMimeLookup)
import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.HTML.DOM                 (eventConduit)
import           Text.XML                      (fromEvents)

upload :: (MonadResource m)
       => Env
       -> Text
       -> Text
       -> Consumer ByteString m ()
upload env bucket name = do
    let mime = defaultMimeLookup name

    body <- compress 9 (WindowBits 31) =$= sinkLazy

    let po = set poContentType (Just $ decodeUtf8 mime)
           $ set poContentEncoding (Just "gzip")
           $ set poCacheControl (Just "maxage=31536000")
           $ set poACL (Just PublicRead)
           $ putObject (toBody body) bucket name
    putStrLn $ "Sending " ++ name
    eres <- liftResourceT $ send env po
    case eres of
        Left e -> error $ show e
        Right _ -> return ()

-- | Uses 'getEnv' for S3 credentials.
uploadDocs :: FilePath -- ^ directory containing docs
           -> Text -- ^ name of current docs, used as prefix in object names
           -> Text -- ^ bucket name
           -> IO ()
uploadDocs input' name bucket = do
    env <- getEnv NorthVirginia Discover

    unlessM (F.isDirectory input') $ error $ "Could not find directory: " ++ show input'
    input <- fmap (</> "") $ F.canonicalizePath input'

    runResourceT $ flip runReaderT (env, bucket) $ flip evalStateT mempty $ do
        hoogles <- execWriterT $ sourceDirectoryDeep False input $$ mapM_C (go input name)
        lbs <- liftIO $ fmap Tar.write $ mapM toEntry $ hoogles []
        upload' (name ++ "/hoogle/orig.tar.gz") $ sourceLazy lbs =$= compress 9 (WindowBits 31)

-- | Create a TAR entry for each Hoogle txt file. Unfortunately doesn't stream.
toEntry :: FilePath -> IO Tar.Entry
toEntry fp = do
    tp <- either error return $ Tar.toTarPath False $ fpToString $ F.filename fp
    Tar.packFileEntry (fpToString fp) tp

upload' :: (MonadResource m, MonadReader (Env, Text) m)
        => Text -- ^ S3 key
        -> Source (ResourceT IO) ByteString
        -> m ()
upload' name src = do
    (env, bucket) <- ask
    liftResourceT $ src $$ upload env bucket name

go :: M m
   => FilePath -- ^ prefix for all input
   -> Text -- ^ upload name
   -> FilePath -- ^ current file
   -> m ()
go input name fp
    | hasExtension fp "txt" = tell (fp:)
    | hasExtension fp "html" = do
        doc <- sourceFile fp
            $= eventConduit
            $= (do
                    yield (Nothing, EventBeginDoctype "html" Nothing)
                    yield (Nothing, EventEndDoctype)
                    mapMC $ \e -> do
                        e' <- goEvent fp toRoot e
                        return (Nothing, e')
                    )
            $$ fromEvents

        -- Sink to a Document and then use blaze-html to render to avoid using
        -- XML rendering rules (e.g., empty elements)
        upload' key $ sourceLazy (renderHtml $ toHtml doc)
    | any (hasExtension fp) $ words "css js png svg gif" = void $ getName fp
    | otherwise = upload' key $ sourceFile fp
  where
    Just suffix = F.stripPrefix input fp
    toRoot = concat $ asList $ replicate (length $ F.splitDirectories suffix) $ asText "../"
    key = name ++ "/" ++ fpToText suffix

goEvent :: M m
        => FilePath -- HTML file path
        -> Text -- ^ relative prefix to root
        -> Event
        -> m Event
goEvent htmlfp toRoot (EventBeginElement name attrs) =
    EventBeginElement name <$> mapM (goAttr htmlfp toRoot) attrs
goEvent _ _ e = return e

goAttr :: M m
       => FilePath -- ^ HTML file path
       -> Text -- ^ relative prefix to root
       -> (Name, [Content])
       -> m (Name, [Content])
goAttr htmlfp toRoot pair@(name, [ContentText value])
    | isRef name && not (".html" `isSuffixOf` value) = do
        let fp = F.parent htmlfp </> fpFromText value
        exists <- liftIO $ F.isFile fp
        if exists
            then do
                x <- getName fp
                return (name, [ContentText $ toRoot ++ x])
            else return pair
goAttr _ _ pair = return pair

isRef :: Name -> Bool
isRef "href" = True
isRef "src" = True
isRef _ = False

type M m = ( MonadReader (Env, Text) m
           , MonadResource m
           , MonadState (Map FilePath Text) m
           , MonadWriter ([FilePath] -> [FilePath]) m
           )

getName :: M m => FilePath -> m Text
getName src = do
    m <- get
    case lookup src m of
        Just x -> return x
        Nothing -> do
            x <- toHash src
            modify $ asMap . insertMap src x
            return x

toHash :: M m => FilePath -> m Text
toHash src = do
    (digest, lbs) <- sourceFile src $$ sink
    let hash' = decodeUtf8 $ B16.encode $ toBytes (digest :: Digest SHA256)
        name = fpToText $ F.addExtensions ("byhash" </> fpFromText hash') (F.extensions src)
    upload' name $ sourceLazy lbs
    return name
  where
    sink = getZipSink $ (,)
        <$> ZipSink sinkHash
        <*> ZipSink sinkLazy

type Setter s a = (a -> Identity a) -> s -> Identity s

set :: Setter s a -> a -> s -> s
set l a s = runIdentity $ l (const $ Identity a) s
