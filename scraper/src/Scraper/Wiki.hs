{-# LANGUAGE OverloadedStrings #-}
module Scraper.Wiki where

import Import
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as TL
import Network.Wreq.StringLess
import Text.Taggy.Lens
import Control.Lens (ix, only, (^..), (^?))

import Data.Aeson.Text (encodeToLazyText)

putSongsJson :: Text -> [SongsByFoot] -> IO ()
putSongsJson lastUpdatedText = writeFileUtf8 (config ^. #output)
  . TL.toStrict
  . encodeToLazyText
  . Envelope lastUpdatedText

mkSongs :: IO [SongsByFoot]
mkSongs = forM (config ^. #feet) $ \feet -> do
  res <- get $ feet ^. #url
  return $ SongsByFoot (feet ^. #foot) (mkSong res)

mkSong :: Response LByteString -> [Song]
mkSong res = catMaybes $ res ^.. responseBody
  . to (TL.fromStrict . decodeUtf8Lenient . toStrictBytes)
  . html
  . allNamed (only "div")
  . attributed (ix "id" . only "wikibody")
  . elements
  . named (only "table")
  . elements
  . named (only "tr")
  . children
  . to table

table :: [Node] -> Maybe Song
table row = Song
  <$> row ^? ix 0 . elements . contents
  <*> row ^? ix 1 . contents
  <*> (Just . Text.concat $ row ^.. ix 2 . contents)
  <*> row ^? ix 3 . contents
