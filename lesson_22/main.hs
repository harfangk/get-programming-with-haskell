{-# LANGUAGE LambdaCase #-}

import Data.List
import System.Exit

main = do
  args <- mapM (const getLine) [1 .. 3]
  mapM_ putStrLn args

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n action = mapM (const action) [1 .. n]

reverser = do
  inputs <- getContents
  let reversed = reverse inputs
  putStrLn reversed

sumOfSquares = do
  inputs <- getContents
  let numbers = toInts inputs
  let squares = map (^ 2) numbers
  print . sum $ squares

toInts :: String -> [Int]
toInts = map read . lines

solveEquation :: String -> Int
solveEquation s =
  if operator == "*"
    then x * y
    else x + y
  where
    parts = words s
    x = read . head $ parts
    firstTail = tail parts
    operator = head firstTail
    furtherTail = tail firstTail
    y = read . head $ furtherTail

simpleCalc = do
  inputs <- getContents
  print . solveEquation $ inputs

quotes :: IO ()
quotes = do
  input <- getContents
  let nums :: [Int] = map read (lines input)
  mapM_ numHandler nums
  where
    numHandler =
      \case
        1 -> do
          putStrLn "This is Sparta!"
          putStrLn "Would you like another?"
        2 -> do
          putStrLn "But I don't want to play as Pontus!"
          putStrLn "Would you like another?"
        3 -> do
          putStrLn "I dig a hole you build a wall"
          putStrLn "Would you like another?"
        4 -> do
          putStrLn "Deus vult!"
          putStrLn "Would you like another?"
        5 -> do
          putStrLn "Interesting"
          putStrLn "This is it!"
          System.Exit.exitSuccess
