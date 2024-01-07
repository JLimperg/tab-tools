{-# LANGUAGE DataKinds #-}
module Types where

import           Data.Text (Text)
import           Network.HTTP.Req (Url, Scheme(Https))

newtype Token = Token { fromToken :: Text }

data TabbycatInstance = TabbycatInstance
  { host :: Text
  , tournament :: Text
  }
  deriving (Read, Show, Eq, Ord)

data Participant = Participant
  { name :: Text
  , urlKey :: Text
  , privateUrl :: Url 'Https
  }
  deriving (Show)

instance Eq Participant where
  p == q = name p == name q

instance Ord Participant where
  compare p q = compare (name p) (name q)
