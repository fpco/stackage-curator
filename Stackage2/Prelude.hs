{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
module Stackage2.Prelude
    ( module X
    , module Stackage2.Prelude
    ) where

import           ClassyPrelude.Conduit as X
import           Data.Conduit.Process  as X
import           Data.Typeable         (TypeRep, typeOf)
import           Distribution.Package  as X (PackageName (PackageName))
import qualified Distribution.Text     as DT
import           Distribution.Version  as X (Version (..))
import           System.Exit           (ExitCode (ExitSuccess))

unPackageName :: PackageName -> Text
unPackageName (PackageName str) = pack str

mkPackageName :: Text -> PackageName
mkPackageName = PackageName . unpack

display :: (IsString text, Element text ~ Char, DT.Text a) => a -> text
display = fromString . DT.display

simpleParse :: (MonadThrow m, DT.Text a, Typeable a, MonoFoldable text, Element text ~ Char)
            => text -> m a
simpleParse orig = withTypeRep $ \rep ->
    case DT.simpleParse str of
        Nothing -> throwM (ParseFailed rep (pack str))
        Just v  -> return v
  where
    str = unpack orig

    withTypeRep :: Typeable a => (TypeRep -> m a) -> m a
    withTypeRep f =
        res
      where
        res = f (typeOf (unwrap res))

        unwrap :: m a -> a
        unwrap _ = error "unwrap"

data ParseFailed = ParseFailed TypeRep Text
    deriving (Show, Typeable)
instance Exception ParseFailed

data ProcessExitedUnsuccessfully = ProcessExitedUnsuccessfully CreateProcess ExitCode
    deriving Typeable
instance Show ProcessExitedUnsuccessfully where
    show (ProcessExitedUnsuccessfully cp ec) = concat
        [ "Process exited with "
        , show ec
        , ": "
        , showCmdSpec (cmdspec cp)
        ]
      where
        showCmdSpec (ShellCommand str) = str
        showCmdSpec (RawCommand x xs) = unwords (x:xs)
instance Exception ProcessExitedUnsuccessfully

checkExitCode :: MonadThrow m => CreateProcess -> ExitCode -> m ()
checkExitCode _ ExitSuccess = return ()
checkExitCode cp ec = throwM $ ProcessExitedUnsuccessfully cp ec

-- FIXME move into streaming-commons?
withCheckedProcess cp f = do
    (x, y, z, sph) <- streamingProcess cp
    res <- f x y z
    ec <- waitForStreamingProcess sph
    checkExitCode cp ec
    return res