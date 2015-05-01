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
import qualified Data.Text                  as T
import           Filesystem                 (listDirectory)
import           Stackage.Prelude
import           System.Directory           (findExecutable)

addDeepDepends :: PackageName -> StateT (Map PackageName Version) IO ()
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
            version <- withCheckedProcess cp $ \ClosedStream src Inherited ->
                src $$ decodeUtf8C =$ linesUnboundedC =$ getZipSink (
                       ZipSink (dependsConduit =$ dependsSink)
                    *> ZipSink versionSink)
            modify $ insertMap name version
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
    dependsSink = mapM_C $ \t -> do
        pn <- simpleParse $ getPackageName t
        addDeepDepends pn

    -- Strip off the hash and version number
    getPackageName =
        reverse . dropSeg . dropSeg . reverse . dropWhile (== ' ')
      where
        dropSeg = drop 1 . dropWhile (/= '-')

-- | Get a @Map@ of all of the core packages. Core packages are defined as
-- packages which ship with GHC itself.
--
-- Precondition: GHC global package database has only core packages, and GHC
-- ships with just a single version of each packages.
getCorePackages :: IO (Map PackageName Version)
getCorePackages = flip execStateT mempty $ mapM_ (addDeepDepends . PackageName)
    [ "ghc"
    , "haskell2010"
    , "haskell98"
    ]

-- | A list of executables that are shipped with GHC.
getCoreExecutables :: IO (Set ExeName)
getCoreExecutables = do
    mfp <- findExecutable "ghc"
    dir <-
        case mfp of
            Nothing -> error "No ghc executable found on PATH"
            Just fp -> return $ directory $ fpFromString fp
    (setFromList . map (ExeName . fpToText . filename)) <$> listDirectory dir

getGhcVersion :: IO Version
getGhcVersion = do
    withCheckedProcess (proc "ghc" ["--numeric-version"]) $
        \ClosedStream src Inherited ->
            (src $$ decodeUtf8C =$ foldC) >>= simpleParse
