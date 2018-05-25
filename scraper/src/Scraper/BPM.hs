{-# LANGUAGE OverloadedStrings #-}

module Scraper.BPM where

import RIO hiding (many)
import qualified RIO.Text as Text
import Text.Parsec
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int

type BPM = (Text, [(Int, Text)])

readBPM :: Text -> BPM
readBPM text = case parse bpm "bpm" text of
  Left _ -> ("", []) -- log?
  Right b -> b

showBPM :: BPM -> LText
showBPM (hd, xs) = toLazyText $ fromText hd <> mconcat (map (\(i, t) -> decimal i <> fromText t) xs)

bpm :: Parsec Text u BPM
bpm = (,) <$> symbol <*> many1 tuple
  where
    tuple = (,) <$> int <*> symbol

int :: Parsec Text u Int
int = fromMaybe 0 . readMaybe <$> many1 digit

symbol :: Parsec Text u Text
symbol = Text.pack <$> many (oneOf "(-)")

example :: Text
example = "55-220(-880)"
