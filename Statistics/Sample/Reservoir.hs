{-# LANGUAGE FlexibleContexts #-}

module Reservoir
    (
      reservoir
    , addElem
    , elems
    ) where

import Control.Monad (liftM, when)
import Control.Monad.Primitive (PrimState)
import Data.Array.IO
import Data.Array.MArray
import Data.IORef
import System.Random.MWC

-- newtype ReservoirState e = ReservoirState (IOUArray Int e)

type ReservoirState e = IOUArray Int e
type ReservoirGenerator = Gen (PrimState IO)

data ReservoirSize = ReservoirSize { current :: Int, limit :: Int } deriving (Eq, Show)
data Reservoir e = Reservoir { size :: IORef (ReservoirSize), unRes :: ReservoirState e, gen :: ReservoirGenerator }

getSize res = readIORef $ size res

incrementReservoirSize res = res { current=(succ $ current res) }

reservoirSize size = ReservoirSize 0 size

elems res = do
    es <- getElems $ unRes res
    size <- current `liftM` (getSize res)
    return $ take size es

reservoir size = do
    arr <- newListArray (0, size - 1) []
    gen <- create
    size <- newIORef $ reservoirSize size
    return $ Reservoir size arr gen

addElem res e = do
    resSize <- getSize res
    pos <- if' (current resSize < limit resSize) (return $ current resSize) (randomPos $ current resSize)
    putStrLn $ "inserting at " ++ show pos
    when (pos < limit resSize) (insertAt res pos e)
    modifyIORef (size res) incrementReservoirSize
    return ()
  where
    randomPos limit = uniformR (0, limit) (gen res)
    insertAt res ix e = do
        writeArray (unRes res) ix e

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
