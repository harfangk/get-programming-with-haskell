{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO
import Lib
import System.Environment
import System.Random

bcInt :: BC.ByteString
bcInt = "6"

t :: Int
t = read . BC.unpack $ bcInt

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC n = BC.pack [intToChar n]

k = do
  n <- randomRIO (0, 255)
  let char = intToChar n
  print char

count = do
  args <- getArgs
  let fileName = head args
  file <- TIO.readFile fileName
  let bc = BC.pack . T.unpack $ file
  let utf8 = E.encodeUtf8 file
  let utf8Length = BC.length utf8
  let byteLength = BC.length bc
  BC.putStrLn utf8
  BC.putStrLn bc
  putStrLn ("Utf8 length: " <> show utf8Length <> ", Byte length: " <> show byteLength)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <-
    foldM
      (\bytes func -> func bytes)
      imageFile
      glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "All done"
