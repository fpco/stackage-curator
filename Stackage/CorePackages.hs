{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stackage.CorePackages
    ( getCorePackages
    , getCoreExecutables
    , getGhcVersion
    ) where

import           Control.Monad.State.Strict (StateT, execStateT, get, modify,
                                             put)
import qualified Data.Map.Lazy              as Map
import           Filesystem                 (listDirectory)
import qualified Filesystem.Path.CurrentOS  as F
import           Stackage.Prelude
import           System.Directory           (findExecutable)
import           System.FilePath            (takeDirectory, takeFileName)

addDeepDepends :: PackageName -> StateT (Map PackageName (Version, Set Text)) IO ()
addDeepDepends name@(PackageName name') = do
    m <- get
    case lookup name m of
        Just _ -> return ()
        Nothing -> do
            -- Specifically use a lazy Map insert since we inject bottom as a
            -- value.  If anyone's curious as the presence of the bottom: we
            -- need to insert something to avoid cycles. We could keep a
            -- separate Set of already-traversed packages, but this is easier
            -- (if a bit hackier).
            put $ Map.insert name (error "Version prematurely forced") m
            let cp = proc "ghc-pkg" ["--no-user-package-conf", "describe", name']
            info <- withCheckedProcess cp $ \ClosedStream src Inherited ->
                src $$ decodeUtf8C =$ linesUnboundedC =$ getZipSink (
                       ZipSink (dependsConduit =$ dependsSink)
                    *> ((,)
                            <$> ZipSink versionSink
                            <*> ZipSink modulesSink))
            modify $ insertMap name info
  where
    -- This sink finds the first line starting with "version: " and parses the
    -- value
    versionSink =
        loop
      where
        loop = await >>= maybe (error "version: not found") go

        go t =
            case stripPrefix "version: " t of
                Nothing -> loop
                Just x -> simpleParse x

    -- Grab the info from the exposed-modules bit
    modulesSink = do
        dropWhileC $ not . ("exposed-modules:" `isPrefixOf`)
        dropC 1
        fmap (setFromList . words . filter (/= ',') . toStrict) $ takeWhileC (" " `isPrefixOf`) .| sinkLazy

    -- Finds the beginning of the depends: block and parses the value. Lots of
    -- ugly text hacking here to try and be compatible with multiple versions
    -- of GHC.
    dependsConduit = do
       dropWhileC $ not . ("depends:" `isPrefixOf`)
       takeWhileC isGood =$= concatMapC sanitize
      where
        -- GHC 7.8 puts a package on the first line with "depends:", GHC 7.10
        -- does not. We want to take all lines that have a dependency and then
        -- stop. This finds them.
        isGood t = "depends:" `isPrefixOf` t || " " `isPrefixOf` t

    -- Strip off: leading whitespace, the word buildin_rts for some reason, and
    -- the depends:. If we end up with an empty line or a line with just
    -- builtin_rts, ignore it.
    sanitize t1
        | null t2 = Nothing
        | t2 == "builtin_rts" = Nothing
        | otherwise = Just t2
      where
        t2 = dropPrefixMaybe "builtin_rts " $ dropPrefixMaybe "depends:" t1

        dropPrefixMaybe x y' =
            fromMaybe y $ stripPrefix x y
          where
            y = dropWhile (== ' ') y'

    -- For each dependency we find: parse it to a package name and then add its
    -- dependencies.
    --
    -- Also: break up multiple packages per line, and strip off the hash and
    dependsSink = mapM_C $ \t' -> forM_ (words t') $ \t -> unless (null t) $ do
        pn <- simpleParse $ getPackageName t
        addDeepDepends pn

    -- Strip off the hash and version number
    getPackageName t0 =
        reverse . dropSegs . reverse . dropWhile (== ' ') $ t0
      where
        dropSegs t
          | null y = t
          | Just y' <- stripPrefix "-" y =
                 if all isVersionChar x
                     then y'
                     else dropSegs y'
          | otherwise = error $ "Got confused in getPackageName on: " ++ show t0
          where
            (x, y) = break (== '-') t
            isVersionChar c = c == '.' || ('0' <= c && c <= '9')

-- | Get a @Map@ of all of the core packages. Core packages are defined as
-- packages which ship with GHC itself.
--
-- Precondition: GHC global package database has only core packages, and GHC
-- ships with just a single version of each packages.
getCorePackages :: IO (Map PackageName (Version, Set Text))
getCorePackages = flip execStateT mempty $ mapM_ (addDeepDepends . PackageName)
    [ "ghc"
    {-
    , "haskell2010"
    , "haskell98"
    -}
    ]

-- | A list of executables that are shipped with GHC.
getCoreExecutables :: IO (Set ExeName)
getCoreExecutables = do
    mfp <- findExecutable "ghc"
    dir <-
        case mfp of
            Nothing -> error "No ghc executable found on PATH"
            Just fp -> return $ takeDirectory fp
    (setFromList . map (ExeName . pack . takeFileName . F.encodeString)) <$> listDirectory (fromString dir)

getGhcVersion :: IO Version
getGhcVersion = do
    withCheckedProcess (proc "ghc" ["--numeric-version"]) $
        \ClosedStream src Inherited ->
            (src $$ decodeUtf8C =$ foldC) >>= simpleParse
