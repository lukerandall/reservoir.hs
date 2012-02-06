{-# LANGUAGE FlexibleContexts #-}

module Reservoir where

import Data.Array.MArray
import Data.Array.IO
import System.Random.MWC
import Control.Monad.Primitive (PrimMonad, PrimState)

data Reservoir e = Reservoir { currentSize :: Int, maxSize :: Int, unRes :: IOUArray Int e, gen :: Gen (PrimState IO) }

newReservoir size = do
    arr <- newListArray (0, size - 1) []
    gen <- create
    return $ Reservoir 0 size arr gen

addElem :: MArray IOUArray t IO => Reservoir t -> t -> IO (Reservoir t)
addElem res e =
    if' (current < limit) (insertAt res current e) (randomPos >>= \pos -> if' (pos < limit) (insertAt res pos e) (increment res))
  where
    current = currentSize res
    limit = maxSize res
    randomPos = uniformR (0, limit) (gen res)
    insertAt res ix e = do
        writeArray (unRes res) ix e
        increment res
    increment res = do
        return $ res { currentSize = (current + 1) }

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
