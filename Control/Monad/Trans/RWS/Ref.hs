{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | An implementation of @RWST@ built on top of mutable references,
-- providing a proper monad morphism.
--
-- An additional advantage of this transformer over the standard @RWST@
-- transformers in the transformers package is that it does not have space
-- leaks in the writer component. For more information, see
-- <https://mail.haskell.org/pipermail/libraries/2012-October/018599.html>.
--
-- Please see the documentation at
-- <https://www.stackage.org/package/monad-unlift> for more details on using
-- this module.
module Control.Monad.Trans.RWS.Ref
    ( RWSRefT
    , runRWSRefT
    , module Control.Monad.RWS.Class
    ) where

import           ClassyPrelude.Conduit
import           Control.Monad.Catch (throwM)
import           Control.Monad.RWS.Class
import           Control.Monad.Trans.Resource (MonadResource (..))

-- |
--
-- @since 0.1.0
newtype RWSRefT r w s m a = RWSRefT
    { unRWSRefT :: r -> IORef w -> IORef s -> m a
    }
    deriving Functor

-- |
--
-- @since 0.1.0
runRWSRefT
    :: ( MonadIO m
       , Monoid w
       )
    => RWSRefT r w s m a
    -> r
    -> s
    -> m (a, s, w)
runRWSRefT (RWSRefT f) r s0 = do
    (refw, refs) <- liftIO $ (,) <$> newIORef mempty <*> newIORef s0
    a <- f r refw refs
    (w, s) <- liftIO $ (,) <$> readIORef refw <*> readIORef refs
    return (a, s, w)
{-# INLINEABLE runRWSRefT #-}

instance Applicative m => Applicative (RWSRefT r w s m) where
    pure m = RWSRefT $ \_ _ _ -> pure m
    {-# INLINE pure #-}
    RWSRefT f <*> RWSRefT g = RWSRefT $ \x y z -> f x y z <*> g x y z
    {-# INLINE (<*>) #-}
instance Monad m => Monad (RWSRefT r w s m) where
    return m = RWSRefT $ \_ _ _ -> return m
    {-# INLINE return #-}
    RWSRefT f >>= g = RWSRefT $ \x y z -> do
        a <- f x y z
        unRWSRefT (g a) x y z
    {-# INLINE (>>=) #-}

instance Monad m => MonadReader r (RWSRefT r w s m) where
    ask = RWSRefT $ \r _ _ -> return r
    {-# INLINE ask #-}
    local f (RWSRefT g) = RWSRefT $ \r w s -> g (f r) w s
instance ( MonadIO m
         , Monoid w
         )
  => MonadWriter w (RWSRefT r w s m) where
    writer (a, w) = RWSRefT $ \_ ref _ ->
        liftIO $ modifyRef' ref (`mappend` w) >> return a
    {-# INLINE writer #-}
    tell w = RWSRefT $ \_ ref _ -> liftIO $ modifyRef' ref (`mappend` w)
    {-# INLINE tell #-}
    listen (RWSRefT f) = RWSRefT $ \r _ s -> do
        ref <- liftIO (newRef mempty)
        a <- f r ref s
        w <- liftIO (readRef ref)
        return (a, w)
    {-# INLINEABLE listen #-}
    pass (RWSRefT f) = RWSRefT $ \r ref s -> do
        (a, g) <- f r ref s
        liftIO $ modifyRef' ref g
        return a
    {-# INLINEABLE pass #-}
instance (MonadIO m) => MonadState s (RWSRefT r w s m) where
    get = RWSRefT $ \_ _ -> liftIO . readRef
    {-# INLINE get #-}
    put x = seq x $ RWSRefT $ \_ _ -> liftIO . (`writeRef` x)
    {-# INLINE put #-}
instance (MonadIO m , Monoid w) => MonadRWS r w s (RWSRefT r w s m)

instance MonadTrans (RWSRefT r w s) where
    lift f = RWSRefT $ \_ _ _ -> f
    {-# INLINE lift #-}
instance MonadIO m => MonadIO (RWSRefT r w s m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}
instance MonadUnliftIO m => MonadUnliftIO (RWSRefT r w s m) where
    withRunInIO inner =
      RWSRefT $ \r w s -> withRunInIO $ \runInIO ->
      inner $ \(RWSRefT f) -> runInIO $ f r w s
instance MonadThrow m => MonadThrow (RWSRefT r w s m) where
    throwM = lift . throwM
instance MonadResource m => MonadResource (RWSRefT r w s m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}
