{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Control.Monad.Request.Lazy
Copyright   :  (c) Tom Hulihan <hulihan.tom159@gmail.com> 2014,
License     :  MIT

Maintainer  :  hulihan.tom159@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-parameter type classes)

[Computation type:] Compuations that send requests and handle responses

[Binding strategy:] Response callbacks are composed with the binding function

[Useful for:] Implementation-agnostic requests (i.e. making real requests versus
mocking), adding middlewares.

[Example type:] @'Request' String String a@

The Request monad
-}
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

import Control.Monad.Request.Class
import Control.Monad.Trans.Free
import Data.Functor.Identity

--------------------------------------------------------------------------------
-- 'Request' and its associated functions

-- | A Request monad, parameterized by the request type, @r@, and response type,
-- @r'@.
-- together.
type Request r r' = RequestT r r' Identity

-- | Turn a request and response callback into a monadic computation.
request :: r              -- ^ The request
        -> (r' -> a)      -- ^ The response callback
        -> Request r r' a -- ^ The resulting computation
request r g = requestT r (return . g)

-- | Evaluate a @'Request' r r\' a@ action.
runRequest :: Request r r' a -- ^ The computation to run
           -> (r -> r')      -- ^ A function that turns requests into responses
           -> a              -- ^ The final result of the computation
runRequest act f = runIdentity $ runRequestT act (Identity . f)

-- | Given a @x -> r@, transform a computation that sends requests of type @x@
-- into one that sends requests of type @r@.
mapRequest :: (x -> r)        -- ^ The middleware function
           -> Request x r' a  -- ^ The computation which sends @x@
           -> Request r r' a  -- ^ The computation which sends @r@
mapRequest f = mapRequestT (return . f)

-- | Given a mapping from @r\' -> x@, transform a computation handles responses
-- of type @x@ to one that handles responses of type @r'@.
mapResponse :: (r' -> x)      -- ^ The middleware function
            -> Request r x a  -- ^ The computation which handles @x@
            -> Request r r' a -- ^ The computation which handles @r'@
mapResponse f = mapResponseT (return . f)

--------------------------------------------------------------------------------
-- 'RequestT' and its associated functions

-- | A request monad, parameterized by the request type, @r@, response type,
-- @r'@, and inner monad, @m@.
type RequestT r r' = FreeT (RequestF r r')

-- | This function takes a request and monadic response handler to produce a
-- @'RequestT' r r\' m a@.
requestT :: Monad m => r                         -- ^ The request
                    -> (r' -> RequestT r r' m a) -- ^ The response callback
                    -> RequestT r r' m a         -- ^ The resulting computation
requestT r g = send r >>= g

-- | Given a @'RequestT' r r\' m a@ and a mapping from requests to responses,
-- return a monadic computation which produces @a@.
runRequestT :: Monad m => RequestT r r' m a -- ^ The computation to run
                       -> (r -> m r')       -- ^ The request function
                       -> m a               -- ^ The resulting computation
runRequestT m f = iterT (\(Request r g) -> f r >>= g) m

-- | Turn a computation that requests @x@ into a computation that requests @r@.
mapRequestT :: Monad m => (x -> RequestT r r' m r) -- ^ The middleware
                       -> RequestT x r' m a        -- ^ The @x@-requester
                       -> RequestT r r' m a        -- ^ The @r@-requester
mapRequestT f = iterTM (\(Request x g) -> f x >>= send >>= g)

-- | Turn a computation that handles @x@ into a computation that handles @r'@.
mapResponseT :: Monad m => (r' -> RequestT r r' m x) -- ^ The middleware
                        -> RequestT r x m a          -- ^ The @x@-handler
                        -> RequestT r r' m a         -- ^ The @r'@-handler
mapResponseT f = iterTM (\(Request r g) -> send r >>= f >>= g)

--------------------------------------------------------------------------------
-- Type class instances from this library.

instance Monad m => MonadRequest r r' (RequestT r r' m) where
    send r = liftF (Request r id)

-- This is the base Functor used to construct 'RequestT'.
data RequestF r r' f = Request r (r' -> f)

instance Functor (RequestF r r') where
    fmap f (Request r g) = Request r (f . g)
