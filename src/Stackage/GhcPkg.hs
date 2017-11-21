{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | General commands related to ghc-pkg.

module Stackage.GhcPkg
    ( setupPackageDatabase
    ) where

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import qualified Data.Conduit.Text as CT
import           Data.Maybe
import qualified Data.Text as T
import           Distribution.Compat.ReadP
import           Distribution.Package
import           Distribution.Text (parse)
import qualified Filesystem.Path.CurrentOS as FP
import Stackage.Prelude
import Filesystem (removeTree)

setupPackageDatabase
    :: Maybe FilePath -- ^ database location, Nothing if using global DB
    -> FilePath -- ^ documentation root
    -> (ByteString -> IO ()) -- ^ logging
    -> Map PackageName Version -- ^ packages and versions to be installed
    -> (PackageIdentifier -> IO ()) -- ^ callback to be used when unregistering a package
    -> IO (Map PackageName Version) -- ^ packages remaining in the database after cleanup
setupPackageDatabase mdb docDir log' toInstall onUnregister = do
    registered1 <- getRegisteredPackages flags
    log' "Unregistering packages with version mismatch\n"
    forM_ registered1 $ \pi'@(PackageIdentifier name version) ->
        case lookup name toInstall of
            Just version' | version /= version' -> unregisterPackage log' onUnregister docDir flags pi'
            _ -> return ()
    log' "\nUnregistering packages which are now broken in the database\n"
    broken <- getBrokenPackages flags
    forM_ broken $ unregisterPackage log' onUnregister docDir flags
    foldMap (\(PackageIdentifier name version) -> singletonMap name version)
        <$> getRegisteredPackages flags
  where
    flags = ghcPkgFlags mdb

ghcPkgFlags :: Maybe FilePath -> [String]
ghcPkgFlags mdb =
    "--no-user-package-db" :
    case mdb of
        Nothing -> ["--global"]
        Just fp -> ["--package-db=" ++ fp]

-- | Get broken packages.
getBrokenPackages :: [String] -> IO [PackageIdentifier]
getBrokenPackages flags = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc
                       "ghc-pkg"
                       ("check" : "--simple-output" : flags))
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Get available packages.
getRegisteredPackages :: [String] -> IO [PackageIdentifier]
getRegisteredPackages flags = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc
                       "ghc-pkg"
                       ("list" : "--simple-output" : flags))
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Parse a package identifier: foo-1.2.3
parsePackageIdent :: Text -> Maybe PackageIdentifier
parsePackageIdent = fmap fst .
    listToMaybe .
    filter (null . snd) .
    readP_to_S parse . T.unpack

-- | Unregister a package.
unregisterPackage :: (ByteString -> IO ()) -- ^ log func
                  -> (PackageIdentifier -> IO ()) -- ^ callback to be used when unregistering a package
                  -> FilePath -- ^ doc directory
                  -> [String] -> PackageIdentifier -> IO ()
unregisterPackage log' onUnregister docDir flags ident@(PackageIdentifier name _) = do
    log' $ "Unregistering " ++ encodeUtf8 (display ident) ++ "\n"
    onUnregister ident

    -- Delete libraries
    (_exitCode, ()) <- sourceProcessWithConsumer
        (proc "ghc-pkg" ("describe" : flags ++ [unpack $ display ident]))
        (CT.decodeUtf8
         $= CT.lines
         $= CL.mapMaybe parseLibraryDir
         $= CL.mapM_ (void . tryIO' . removeTree . FP.decodeString))

    void (readProcessWithExitCode
              "ghc-pkg"
              ("unregister": flags ++ ["--force", unpack $ display name])
              "")

    void $ tryIO' $ removeTree $ FP.decodeString $ docDir </> unpack (display ident)
  where
    parseLibraryDir = fmap unpack . stripPrefix "library-dirs: "

    tryIO' :: IO a -> IO (Either IOException a)
    tryIO' = try
