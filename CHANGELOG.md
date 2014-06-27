# 0.3.0.1

* Change the internal datastructure for `RequestT` from a `data` to a `newtype`

# 0.3.0.0

* Use `FreeT` monad to implement `RequestT`
* Change the type of callback given to `requestT` from `r' -> m a` to `r' -> RequestT r r' m a`

# 0.2.0.0

* Generalize `mapRequestT` and `mapResponseT` so that requests may be performed in middleware

# 0.1.0.0

* First public release
* Add the `MonadRequest` type class
* Add the `RequestT` monad
* Initial documentation
