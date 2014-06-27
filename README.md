# request-monad

[Hackage](http://hackage.haskell.org/package/request-monad)

This library exports a monad that can be used to abstract a request sending/response handling pattern.
It can be used to implement type-transforming middleware, as well as a way to easily implement stubbing.

## Installation

From the command line:

```shell
$ cabal install request-monad
```

To use data types and functions export from this library:

```haskell
import Control.Monad.Request
```

## Usage

Using `RequestT r r' m a` abstracts sending requests of type `r`, and handling responses of `r'`.
Below is an example of using `RequestT` to ask for somebody's name and age.
Note that there is no logic about _how_ to get and return the strings in `getNameAndAge`, that's all handled in `prompt`.

```haskell
import Control.Monad
import Control.Monad.Request
import System.IO

getNameAndAge :: Monad m => RequestT String String m (String, Int)
getNameAndAge = do
    name <- send "Name: "
    age <- liftM read $ send "Age: "
    return (name, age)

prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

main :: IO ()
main = do
    (name, age) <- runRequestT getNameAndAge prompt
    putStrLn $ name ++ " is " ++ show age ++ " years old."
```

Below is an example of an echo server, which just returns the exact input that it was given.

```haskell
import Control.Monad.Request

pingPong :: Monad m => RequestT String String m (String, String)
pingPong = do
    a <- send "ping"
    b <- send "pong"
    return (a, b)

main :: IO ()
main = do
    let (a, b) = runRequest pingPong id
    putStrLn $ "a: " ++ a -- Prints "a: ping"
    putStrLn $ "b: " ++ b -- Prints "b: pong"
```

Aside from implementation-independant requests, this abstraction also simplifies adding request/response middleware.
The code below adds JSON deserialization to each response.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Request
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

deserialize :: (A.FromJSON a, Monad m) => B.ByteString -> m (Maybe a)
deserialize = return . A.decode

tryTwice :: Monad m => RequestT B.ByteString B.ByteString m (Maybe A.Value)
tryTwice = mapResponseT deserialize $ do
    a <- send "request one"
    b <- send "request two"
    return $ a `mplus` b

handleRequest :: Monad m => B.ByteString -> B.ByteString
handleRequest "request one" = return "not json"
handleRequest x             = "[15]"

main :: IO ()
main = do
    let res = runRequest tryTwice handleRequest
    print $ res -- Prints "Just (Array (fromList [Number 15.0]))"
```

## TODO

* Add a strict version of `RequestT`
