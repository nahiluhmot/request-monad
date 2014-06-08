{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module the 'MonadRequest' type class, with instances for other
-- transformers.
module Control.Monad.Request.Class ( MonadRequest(..) ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Error as Error
import Control.Monad.Trans.Except as Except
import Control.Monad.Trans.List as List
import Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.RWS.Lazy as RWSL
import Control.Monad.Trans.RWS.Strict as RWSS
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Lazy as StateL
import Control.Monad.Trans.State.Strict as StateS
import Control.Monad.Trans.Writer.Lazy as WriterL
import Control.Monad.Trans.Writer.Strict as WriterS
import Data.Monoid

-- | Type type class generalizes monadic requests.
--
--  * @r@ - The type of request
--
--  * @r'@ - The type of response
--
--  * @m@ - The monad through which the requests are sent
--
class Monad m => MonadRequest r r' m | m -> r r' where
    -- | Given a request of type @r@, perform an action in @m@ whose result is
    -- @r'@.
    send :: r -> m r'

instance MonadRequest r r' m => MonadRequest r r' (IdentityT m) where
    send = IdentityT . send

instance (MonadRequest r r' m) => MonadRequest r r' (ContT x m) where
    send = lift . send

instance (Error e, MonadRequest r r' m) => MonadRequest r r' (ErrorT e m) where
    send = ErrorT . liftM Right . send

instance MonadRequest r r' m => MonadRequest r r' (ExceptT e m) where
    send = ExceptT . liftM Right . send

instance MonadRequest r r' m => MonadRequest r r' (ListT m) where
    send = ListT . liftM (\x -> [x]) . send

instance MonadRequest r r' m => MonadRequest r r' (MaybeT m) where
    send = MaybeT . liftM Just . send

instance (Monoid w, MonadRequest r r' m) => MonadRequest r r' (RWSL.RWST x w s m) where
    send = lift . send

instance (Monoid w, MonadRequest r r' m) => MonadRequest r r' (RWSS.RWST x w s m) where
    send = lift . send

instance MonadRequest r r' m => MonadRequest r r' (ReaderT x m) where
    send = ReaderT . const . send

instance MonadRequest r r' m => MonadRequest r r' (StateL.StateT x m) where
    send r = StateL.StateT $ \s -> send r >>= \a -> return (a, s)

instance MonadRequest r r' m => MonadRequest r r' (StateS.StateT x m) where
    send r = StateS.StateT $ \s -> send r >>= \a -> return (a, s)

instance (Monoid w, MonadRequest r r' m) => MonadRequest r r' (WriterL.WriterT w m) where
    send = WriterL.WriterT . liftM (flip (,) mempty) . send

instance (Monoid w, MonadRequest r r' m) => MonadRequest r r' (WriterS.WriterT w m) where
    send = WriterS.WriterT . liftM (flip (,) mempty) . send
