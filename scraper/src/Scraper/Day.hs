module Scraper.Day where

import RIO
import RIO.Time
import RIO.Text

getLastUpdated :: IO Text
getLastUpdated = pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing) <$> getCurrentTime
