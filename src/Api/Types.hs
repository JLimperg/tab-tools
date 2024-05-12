{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as Text

newtype Token = Token { fromToken :: Text }

data TabbycatInstance = TabbycatInstance
  { host :: Text
  , tournament :: Text
  }
  deriving (Read, Show, Eq, Ord)

newtype Link = Link { url :: Text }
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

newtype InstitutionCode = InstitutionCode { code :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON)

newtype TeamReference = TeamReference { name :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON)

data FeedbackAnswer
  = FeedbackAnswerBool Bool
  | FeedbackAnswerText Text
  | FeedbackAnswerInt Int
  deriving (Eq, Ord, Read, Show)

instance FromJSON FeedbackAnswer where
  parseJSON = \case
    String t -> pure $ FeedbackAnswerText t
    v@(Number n) ->
      case toBoundedInteger n of
        Nothing -> fail $ "While parsing numeric answer: expected integer, but got: '" ++ show v ++ "'"
        Just i -> pure $ FeedbackAnswerInt i
    Bool b   -> pure $ FeedbackAnswerBool b
    v -> fail $ "While parsing answer: expected string, number or boolean, but got '" ++ show v ++ "'"

renderFeedbackAnswer :: FeedbackAnswer -> Text
renderFeedbackAnswer = \case
  FeedbackAnswerBool b -> Text.pack $ show b
  FeedbackAnswerText t -> t
  FeedbackAnswerInt  n -> Text.pack $ show n
