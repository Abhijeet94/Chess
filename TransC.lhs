---
fulltitle: "Exercise: Concurrency Monad Transformer"
date: November 29, 2017
---

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

> module TransC where

> import Control.Monad.Trans
> import qualified Control.Monad.State as S
> import Control.Monad (ap, liftM)
> import System.IO (hReady, stdin)
> import qualified DList as DL
> import Test.HUnit hiding (State)

> class Monad m => Output m where
>    write :: String -> m ()
> class Monad m => Input m where
>    input :: m (Maybe String)    -- only return input if it is ready

> instance Output IO where
>    write = putStr
> instance Input IO where
>    input = Just <$> getLine

> type FakeIO = S.State FakeState

> data FakeState = FS
>    { fsWrite :: DL.DList String    -- what has been written
>    , fsInput :: [Maybe String]     -- what to read from
>    }

> instance Output FakeIO where
>    write s = do st <- S.get
>                 let oldLog = fsWrite st
>                 let newLog = DL.append oldLog (DL.singleton s)
>                 S.put $ st { fsWrite = newLog }

> instance Input FakeIO where
>    
>    input = do st <- S.get
>               let (v,rest) =  case (fsInput st) of
>                                 []     -> (Nothing,[])
>                                 (x:xs) -> (x,xs)
>               S.put $ st { fsInput = rest }
>               return v
>    

> runFakeIO :: FakeIO () -> [Maybe String] -> [String]
> runFakeIO comp inputs =
>       DL.toList (fsWrite (S.execState comp initState))
>  where
>       initState = FS { fsWrite = DL.empty, fsInput = inputs }


