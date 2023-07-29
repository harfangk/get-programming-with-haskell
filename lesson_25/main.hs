{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import System.Environment

bcInt :: BC.ByteString
bcInt = "6"

t :: Int
t = read . BC.unpack $ bcInt

main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  let glitched = imageFile
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "All done"

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC n = BC.pack [intToChar n]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal)
