{- |
Module      :  Control.Monad.Request.Class
Copyright   :  (c) Tom Hulihan <hulihan.tom159@gmail.com> 2014,
License     :  MIT

Maintainer  :  hulihan.tom159@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-parameter type classes)

[Computation type:] Compuations that send requests and handle responses

[Binding strategy:] Response callbacks are composed with the binding function

[Useful for:] Implementation-agnostic requests (i.e. making real requests versus
mocking), adding middlewares.

[Example type:] @'Control.Monad.Request.Lazy.Request' String String a@

The Request monad
-}
module Control.Monad.Request ( module Control.Monad.Request.Lazy ) where

import Control.Monad.Request.Lazy
