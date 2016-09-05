{-# LANGUAGE FlexibleContexts, GADTs, NoImplicitPrelude, OverloadedStrings,
             ScopedTypeVariables #-}
module Stackage.DiffPlans
    ( diffPlans
    ) where

import           Data.Map (filterWithKey)
import           Data.Text (justifyLeft)
import           Data.Yaml (decodeFileEither)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Stackage.Prelude

import           Data.Maybe
import           Lucid
import           System.Directory

data Change = Added | Deleted | MajorBump | MinorBump | Unchanged
    deriving (Show, Eq, Ord)

data AndOr a = Old a | New a | Both a a
    deriving Show
instance Semigroup (AndOr a) where
    Old x    <> New y    = Both x y
    New y    <> Old x    = Both x y
    Old x    <> Old _    = Old x
    New x    <> New _    = New x
    Both x y <> _        = Both x y
    Old x    <> Both _ y = Both x y
    New y    <> Both x _ = Both x y

type DiffMap = Map Change (Map PackageName (Text,Maybe Text))

diffPlans :: FilePath -- ^ old YAML build plan file
          -> FilePath -- ^ new YAML build plan file
          -> Bool     -- ^ show just changed packages
          -> Bool     -- ^ use colours
          -> Bool     -- ^ fetch YAML files from GitHub repo
          -> Bool     -- ^ wrap output in HTML
          -> IO ()
diffPlans oldFP newFP diffsOnly useColor True asHtml = do
    (oldFP', newFP') <- (,) <$> getLTS oldFP <*> getLTS newFP
    diffPlans oldFP' newFP' diffsOnly useColor False asHtml
    delFile oldFP'
    delFile newFP'

    where
        delFile fp = removeFile fp `catch` \(_::SomeException) -> return ()

diffPlans oldFP newFP diffsOnly useColor False asHtml = do
    old <- fmap Old <$> parse oldFP
    new <- fmap New <$> parse newFP
    let combined = unionWith (<>) old new
        m ::  DiffMap
        m = f . unionsWith mappend . map go $ mapToList combined
        f = if diffsOnly
               then filterWithKey (\k _ -> k /= Unchanged)
               else id

    if asHtml
       then print $ htmlOut True m
       else consoleOut useColor  m
  where
    parse fp = decodeFileEither fp
           >>= either throwIO (return . toSimple)

    toSimple = fmap ppVersion . bpPackages

    go (name, Old x) = singletonMap Deleted   $ singletonMap name (display x, Nothing)
    go (name, New x) = singletonMap Added     $ singletonMap name (display x, Nothing)
    go (name, Both x y)
        | x == y     = singletonMap Unchanged $ singletonMap name (display x, Nothing)
        | otherwise  = singletonMap
                  (if isMajor x y then MajorBump else MinorBump)
                  (singletonMap name $ (display x, Just $ display y))

isMajor :: Version -> Version -> Bool
isMajor (Version old _) (Version new _) =
    toPair old /= toPair new
  where
    toPair []      = (0, 0)
    toPair [i]     = (i, 0)
    toPair (i:j:_) = (i, j)


-- | Download LTS file from GitHub to TMP dir
--    LTS should not contain extension nor path, i.e. just "lts-2.19"
getLTS :: String -> IO FilePath
getLTS lts = do
    createDirectoryIfMissing True tmpDir
    man <- newManager tlsManagerSettings
    req <- parseUrlThrow $ ltsRepo <> lts <> ".yaml"
    res <- httpLbs req man
    writeFile fName $ responseBody res
    return fName
  where
    fName   = tmpDir <> lts <> ".yaml"
    ltsRepo = "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    tmpDir  = "/tmp/stackage-curator/"


-- | Return coloured string, or html colour style, depending on *change* param
colorize :: Bool -> Change -> Text -> Text
colorize useHtml change s =
  case change of
      Deleted   -> red    s
      Added     -> green  s
      Unchanged ->        s
      MajorBump -> yellow s
      MinorBump -> blue   s
  where
      showInColor consCol htmlColor s'
          | useHtml   = "color: " <> htmlColor
          | otherwise = "\ESC["   <> consCol <> "m" <> s' <> "\ESC[0m"

      --black   = showInColor "30" "black"
      red     = showInColor "31" "red"
      green   = showInColor "32" "green"
      yellow  = showInColor "33" "yellow"
      blue    = showInColor "34" "blue"
      --magenta = showInColor "35" "magenta"
      --cyan    = showInColor "36" "cyan"
      --white   = showInColor "37" "white"


-- | Display to console
consoleOut :: Bool ->  DiffMap -> IO ()
consoleOut useColor m =
    forM_ (mapToList m) $ \(change, m') -> do
        print change
        forM_ (mapToList m') $ \(pkg, (x,y)) ->
            let pkgName' = (if useColor then colorize False change else id)
                            $ justifyLeft 25 ' ' $ display pkg
             in putStrLn $ pkgName'             <>
                           justifyLeft 9 ' ' x  <>
                           if isJust y
                              then "  =>  "     <> fromJust y
                              else ""
        putStrLn ""


-- | Display as HTML. If fullPage is True, display as complete page
htmlOut :: Bool -> DiffMap -> Html ()
htmlOut fullPage m  = do
    when fullPage $
        doctypehtml_$ head_ $ do
            meta_ [charset_ "utf-8"]
            style_ "table, th, td {border : 1px solid black; border-collapse: collapse;}\
                   \th, td        {padding: 5px; text-align: left;}"
    body_ $
        div_ [class_ "ltsDiffs"] $ do
            h3_ "Differences"
            forM_ (mapToList m) $ \(change, m') -> do
                p_ [style_ $ colorize True change ""] $ toHtml $ show change
                table_ $ forM_ (mapToList m') $ \(pkg, (x,y)) ->
                    tr_ $ do
                        td_ $ toHtml $ display pkg
                        td_ $ toHtml $ x
                        when (isJust y) $
                            td_ $ toHtml $ fromJust y
                br_ []
