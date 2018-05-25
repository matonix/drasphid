{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Scraper.Config
  ( config
  ) where

import RIO
import Data.Extensible
import qualified Data.Yaml.TH as Y
import Instances.TH.Lift ()

type Config = Record
  '[ "feet" >: [Feet]
   , "output" >: FilePath
   ]

type Feet = Record
 '[ "foot" >: Int
  , "url" >: Text
  ]

config :: Config
config = $$(Y.decodeFile "./resources/config.yaml")
