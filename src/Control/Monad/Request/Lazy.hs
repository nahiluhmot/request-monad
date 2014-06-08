{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Request.Lazy ( -- * MonadRequest
                                    MonadRequest(..)
                                    -- * Request
                                  , Request
                                  , request
                                  , runRequest
                                  , mapRequest
                                  , mapResponse
                                    -- * RequestT
                                  , RequestT
                                  , requestT
                                  , runRequestT
                                  , mapRequestT
                                  , mapResponseT
                                  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Request.Class
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.RWS.Class
import Data.Monoid
import Data.Functor.Identity

--------------------------------------------------------------------------------
-- 'Request' and its associated functions

-- | A Request monad, parameterized by the request type, @r@, and response type,
-- @r'@. 'return' produces a pure value, while '>>=' composes the actions
-- together.
type Request r r' = RequestT r r' Identity

-- | Turn a request and response callback into a monadic computation.
request :: r              -- ^ The request
        -> (r' -> a)      -- ^ The response callback
        -> Request r r' a -- ^ The resulting computation
request r g = requestT r (Identity . g)

-- | Evaluate a 'Request' action.
runRequest :: Request r r' a -- ^ The computation to run
           -> (r -> r')      -- ^ A function that turns requests into responses
           -> a              -- ^ The final result of the computation
runRequest act f = runIdentity $ runRequestT act (Identity . f)

-- | Given a mapping from @x@ to @r@, transform a computation that sends
-- requests of type @x@ to one that sends requests of type @r@.
mapRequest :: (x -> r)        -- ^ The middleware function
           -> Request x r' a  -- ^ The computation which sends @x@
           -> Request r r' a  -- ^ The computation which sends @r@
mapRequest f = mapRequestT (Identity . f)

-- | Given a mapping from @r'@ to @x@, transform a computation handles responses
-- of type @x@ to one that handles responses of type @r'@.
mapResponse :: (r' -> x)      -- ^ The middleware function
            -> Request r x a  -- ^ The computation which handles @x@
            -> Request r r' a -- ^ The computation which handles @r'@
mapResponse f = mapResponseT (Identity . f)

--------------------------------------------------------------------------------
-- 'RequestT' and its associated functions

-- | A Request monad, parameterized by the request type, @r@, response type,
-- @r'@, and inner monad, @m@. 'return' produces a @m a@, while '>>=' composes
-- the actions.
data RequestT r r' m a
        = Pure a
        | Request r (r' -> RequestT r r' m a)
        | Lift (m (RequestT r r' m a))

-- | This function takes a request and monadic response handler to produce a
-- 'RequestT'.
requestT :: Monad m => r                 -- ^ The request
                    -> (r' -> m a)       -- ^ The response callback
                    -> RequestT r r' m a -- ^ The resulting computation
requestT r g = Request r (Lift . liftM Pure . g)

-- | Given a 'RequestT' and a mapping from requests to responses, return a
-- monadic computation which produces @a@.
runRequestT :: Monad m => RequestT r r' m a -- ^ The computation to run
                       -> (r -> m r')       -- ^ The request function
                       -> m a               -- ^ The resulting computation
runRequestT m req =
    let go      (Pure a) = return a
        go (Request r g) = req r >>= go . g
        go    (Lift act) = act >>= go
    in  go m

-- | Turn a computation that requests @x@ into a computation that requests @r@.
mapRequestT :: Monad m => (x -> m r)        -- ^ The middleware function
                       -> RequestT x r' m a -- ^ The @x@-requesting computation
                       -> RequestT r r' m a -- ^ The @r@-requesting computation
mapRequestT f =
    let go      (Pure a) = Pure a
        go (Request x g) = lift (f x) >>= flip Request (go . g)
        go    (Lift act) = Lift (liftM go act)
    in  go

-- | Turn a computation that handles @x@ into a computation that handles @r'@.
mapResponseT :: Monad m => (r' -> m x)       -- ^ The middleware function
                        -> RequestT r x m a  -- ^ The @x@-handling computation
                        -> RequestT r r' m a -- ^ The @r'@-handling computation
mapResponseT f =
    let go      (Pure a) = Pure a
        go (Request r g) = Request r (go . g <=< lift . f)
        go    (Lift act) = Lift (liftM go act)
    in  go

--------------------------------------------------------------------------------
-- Type class instances from base.

instance Alternative m => Alternative (RequestT r r' m) where
    empty = Lift empty
    (<|>) =
        let go      (Pure a) _ = Pure a
            go (Request r g) x = Request r (flip go x . g)
            go    (Lift act) x = Lift (fmap (flip go x) act)
        in  go

instance Applicative m => Applicative (RequestT r r' m) where
    pure = Pure
    (<*>) =
        let go      (Pure f)      (Pure a) = Pure (f a)
            go (Request r g)             x = Request r (flip go x . g)
            go    (Lift act)             x = Lift (fmap (flip go x) act)
            go             x (Request r g) = Request r (go x . g)
            go             x    (Lift act) = Lift (fmap (go x) act)
        in  go

instance Functor m => Functor (RequestT r r' m) where
    fmap f =
        let go      (Pure a) = Pure (f a)
            go (Request r g) = Request r (go . g)
            go    (Lift act) = Lift (fmap go act)
        in  go

instance Monad m => Monad (RequestT r r' m) where
    return = Pure
    fail = Lift . fail
    (>>=) m f =
        let go      (Pure a) = f a
            go (Request r g) = Request r (go . g)
            go    (Lift act) = Lift (liftM go act)
        in  go m

instance MonadPlus m => MonadPlus (RequestT r r' m) where
    mzero = Lift mzero
    mplus =
        let go      (Pure a) _ = Pure a
            go (Request r g) x = Request r (flip go x . g)
            go    (Lift act) x = Lift (liftM (flip go x) act)
        in  go

--------------------------------------------------------------------------------
-- Type class instances from this library.

instance Monad m => MonadRequest r r' (RequestT r r' m) where
    send = flip Request Pure

--------------------------------------------------------------------------------
-- Type class instances from transformers.

instance MonadIO m => MonadIO (RequestT r r' m) where
    liftIO = lift . liftIO

instance MonadTrans (RequestT r r') where
    lift = Lift . liftM Pure

--------------------------------------------------------------------------------
-- Type class instances from mtl.

instance MonadError e m => MonadError e (RequestT r r' m) where
    throwError = Lift . throwError
    catchError m h =
        let go      (Pure a) = Pure a
            go (Request r g) = Request r (go . g)
            go    (Lift act) = Lift (catchError act (return . h))
        in  go m


instance MonadReader x m => MonadReader x (RequestT r r' m) where
    ask = lift ask
    local f =
        let go      (Pure a) = Pure a
            go (Request r g) = Request r (go . g)
            go (Lift act)    = Lift (liftM go (local f act))
        in  go

instance (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (RequestT r r' m)

instance MonadState s m => MonadState s (RequestT r r' m) where
    get = lift get
    put = lift . put

instance (Monoid w, MonadWriter w m) => MonadWriter w (RequestT r r' m) where
    writer = lift . writer
    tell = lift . tell
    listen =
        let go acc      (Pure a) = Pure (a, acc)
            go acc (Request r g) = Request r (go acc . g)
            go acc    (Lift act) = Lift $ do
                ~(m, w) <- listen act
                return (go (acc `mappend` w) m)
        in  go mempty
    pass m = listen m >>= \ ~(~(a, f), w) -> writer (a, f w)
