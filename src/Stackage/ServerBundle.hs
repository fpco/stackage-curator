-- | Create a bundle to be uploaded to Stackage Server.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
module Stackage.ServerBundle
    ( bpAllPackages
    , docsListing
    , createBundleV2
    , CreateBundleV2 (..)
    , SnapshotType (..)
    , writeIndexStyle
    , DocMap
    , PackageDocs (..)
    ) where

import qualified Data.Yaml                 as Y
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.Prelude
import qualified Text.XML                  as X
import           Text.XML.Cursor
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName)

-- | All package/versions in a build plan, including core packages.
--
-- Note that this may include packages not available on Hackage.
bpAllPackages :: BuildPlan -> Map PackageName Version
bpAllPackages BuildPlan {..} =
    siCorePackages bpSystemInfo ++ map ppVersion bpPackages

docsListing :: BuildPlan
            -> FilePath -- ^ docs directory
            -> IO DocMap
docsListing bp docsDir =
    fmap fold $ mapM go $ mapToList $ bpAllPackages bp
  where
    go :: (PackageName, Version) -> IO DocMap
    go (package, version) = do -- handleAny (const $ return mempty) $ do
        let dirname = unpack (concat
                [ display package
                , "-"
                , display version
                ])
            indexFP = (docsDir </> dirname </> "index.html")
        ie <- doesFileExist indexFP
        if ie
            then do
                doc <- flip X.readFile indexFP X.def
                    { X.psDecodeEntities = X.decodeHtmlEntities
                    }
                let cursor = fromDocument doc
                    getPair x = take 1 $ do
                        href <- attribute "href" x
                        let name = concat $ x $// content
                        guard $ not $ null name
                        return (href, name)
                    pairs = cursor $// attributeIs "class" "module"
                                   &/ laxElement "a" >=> getPair
                m <- fmap fold $ forM pairs $ \(href, name) -> do
                    let suffix = dirname </> unpack href
                    e <- doesFileExist $ docsDir </> suffix
                    return $ if e
                        then asMap $ singletonMap name [pack dirname, href]
                        else mempty
                return $ singletonMap (display package) $ PackageDocs
                    { pdVersion = display version
                    , pdModules = m
                    }
            else return mempty

data CreateBundleV2 = CreateBundleV2
    { cb2Plan :: BuildPlan
    , cb2Type :: SnapshotType
    , cb2DocsDir :: FilePath
    , cb2DocmapFile :: !FilePath
    }

-- | Create a V2 bundle, which contains the build plan, metadata, docs, and doc
-- map.
createBundleV2 :: CreateBundleV2 -> IO ()
createBundleV2 CreateBundleV2 {..} = do
    docsDir <- canonicalizePath cb2DocsDir
    docMap <- docsListing cb2Plan cb2DocsDir

    Y.encodeFile (docsDir </> "build-plan.yaml") cb2Plan
    Y.encodeFile (docsDir </> "build-type.yaml") cb2Type
    Y.encodeFile (docsDir </> "docs-map.yaml") docMap
    Y.encodeFile cb2DocmapFile docMap
    void $ writeIndexStyle Nothing cb2DocsDir

writeIndexStyle :: Maybe Text -- ^ snapshot id
                -> FilePath -- ^ docs dir
                -> IO [String]
writeIndexStyle msnapid dir = do
    dirs <- fmap sort
          $ runConduitRes
          $ sourceDirectory dir
         .| filterMC (liftIO . doesDirectoryExist)
         .| mapC takeFileName
         .| sinkList
    writeFile (dir </> "index.html") $ encodeUtf8 $ asText $ pack $ mkIndex
        (unpack <$> msnapid)
        dirs
    writeFile (dir </> "style.css") $ encodeUtf8 $ asText $ pack styleCss
    return dirs

mkIndex :: Maybe String -> [String] -> String
mkIndex msnapid dirs = concat
    [ "<!DOCTYPE html>\n<html lang='en'><head><title>Haddocks index</title>"
    , "<link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css'>"
    , "<link rel='stylesheet' href='style.css'>"
    , "<link rel='shortcut icon' href='https://www.stackage.org/static/img/favicon.ico' />"
    , "</head>"
    , "<body><div class='container'>"
    , "<div class='row'><div class='span12 col-md-12'>"
    , "<h1>Haddock documentation index</h1>"
    , flip foldMap msnapid $ \snapid -> concat
        [ "<p class='return'><a href=\"https://www.stackage.org/stackage/"
        , snapid
        , "\">Return to snapshot</a></p>"
        ]
    , "<ul>"
    , concatMap toLI dirs
    , "</ul></div></div></div></body></html>"
    ]
  where
    toLI name = concat
        [ "<li><a href='"
        , name
        , "/index.html'>"
        , name
        , "</a></li>"
        ]

styleCss :: String
styleCss = concat
    [ "@media (min-width: 530px) {"
    , "ul { -webkit-column-count: 2; -moz-column-count: 2; column-count: 2 }"
    , "}"
    , "@media (min-width: 760px) {"
    , "ul { -webkit-column-count: 3; -moz-column-count: 3; column-count: 3 }"
    , "}"
    , "ul {"
    , "  margin-left: 0;"
    , "  padding-left: 0;"
    , "  list-style-type: none;"
    , "}"
    , "body {"
    , "  background: #f0f0f0;"
    , "  font-family: 'Lato', sans-serif;"
    , "  text-shadow: 1px 1px 1px #ffffff;"
    , "  font-size: 20px;"
    , "  line-height: 30px;"
    , "  padding-bottom: 5em;"
    , "}"
    , "h1 {"
    , "  font-weight: normal;"
    , "  color: #06537d;"
    , "  font-size: 45px;"
    , "}"
    , ".return a {"
    , "  color: #06537d;"
    , "  font-style: italic;"
    , "}"
    , ".return {"
    , "  margin-bottom: 1em;"
    , "}"]
