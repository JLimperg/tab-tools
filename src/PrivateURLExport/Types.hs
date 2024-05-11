{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module PrivateURLExport.Types where

import Data.Text (Text)
import Network.HTTP.Req (Url, Scheme(Https))

data Participant = Participant
  { name :: Text
  , urlKey :: Text
  , privateUrl :: Url 'Https
  }
  deriving (Show)

instance Eq Participant where
  p == q = p.name == q.name

instance Ord Participant where
  compare p q = compare p.name q.name
