{-# LANGUAGE DeriveGeneric #-}

module RenderEmailTable
( Adjudicator(..)
, renderEmailTable )
where

import           Data.ByteString.Lazy (ByteString)
import           Data.Csv
  ( ToNamedRecord, DefaultOrdered(..), encodeDefaultOrderedByName )
import           Data.Text (Text)
import           GHC.Generics (Generic)

data Adjudicator = Adjudicator
  { name :: Text
  , email :: Maybe Text
  , urlKey :: Text
  }
  deriving (Generic)

instance ToNamedRecord Adjudicator
instance DefaultOrdered Adjudicator

renderEmailTable :: [Adjudicator] -> ByteString
renderEmailTable = encodeDefaultOrderedByName
