module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

main :: IO ()
main = print ""

qcArray :: UArray Int Bool
qcArray = array (0, 4) [(1, True), (2, True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) (zip [0 .. 3] (repeat 0))

doubled :: UArray Int Int
doubled = accum (*) beansInBuckets $ zip [0 .. 3] (repeat 2)

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
  let end = length vals - 1
  stArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray stArray i val
  return stArray

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - i)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray

crossover :: Int -> (UArray Int Bool, UArray Int Bool) -> UArray Int Bool
crossover cutoff (a1, a2) = runSTUArray $ do
  stArray <- thaw a1
  let end = snd . bounds $ a1
  forM_ [cutoff .. end] $ \i -> do
    let a2val = a2 ! i
    writeArray stArray i a2val
  return stArray

replaceZeroes :: UArray Int Int -> UArray Int Int
replaceZeroes a = runSTUArray $ do
  stArray <- thaw a
  let end = snd . bounds $ a
  forM_ [0 .. end] $ \i -> do
    val <- readArray stArray i
    when (val == 0) $ do
      writeArray stArray i (-1)
  return stArray
