{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Scraper.Wiki

run :: RIO App ()
run = do
  logInfo "Putting JSON of songs now..."
  lastUpdatedText <- liftIO getLastUpdated
  liftIO $ mkSongs >>= putSongsJson lastUpdatedText
