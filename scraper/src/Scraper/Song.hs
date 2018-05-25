{-# LANGUAGE DeriveGeneric #-}
module Scraper.Song where

import RIO
import Data.Yaml

data Song = Song
  { name :: Text
  , folder :: Text
  , bpm :: Text
  , notes :: Text
  } deriving (Show, Generic)

instance ToJSON Song
