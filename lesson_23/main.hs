{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import Data.Text.Lazy.Read qualified as TLR

myTLines s = T.splitOn "\n"

myTUnlines s = T.intercalate "\n"

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

doHighlight = do
  TIO.putStrLn (highlight dharma bgText)

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

hello :: IO ()
hello = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement

toInts :: TL.Text -> [Int]
toInts t = map f eitherNums
  where
    ls = TL.lines t
    eitherNums = map TLR.decimal ls
    f = \case
      (Left _) -> 0
      (Right (val, rest)) -> val

main :: IO ()
main = do
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  print (sum numbers)
