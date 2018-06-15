{-# LANGUAGE DeriveGeneric #-}
module Scraper.Song where

import RIO
import Data.Aeson

data Envelope = Envelope
  { songsByfoot :: ![SongsByFoot]
  } deriving (Show, Generic)

instance ToJSON Envelope

data SongsByFoot = SongsByFoot
  { foot :: !Int
  , songs :: ![Song]
  } deriving (Show, Generic)

instance ToJSON SongsByFoot

data Song = Song
  { name :: !Text
  , folder :: !Text
  , bpm :: !Text
  , notes :: !Text
  } deriving (Show, Generic)

instance ToJSON Song
