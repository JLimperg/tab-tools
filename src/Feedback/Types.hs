{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Feedback.Types where

import Data.Aeson (Value(..), FromJSON(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)

newtype Token = Token { fromToken :: Text }

data Answer
  = AnswerBool Bool
  | AnswerText Text
  | AnswerInt Int
  deriving (Eq, Ord, Read, Show)

instance FromJSON Answer where
  parseJSON = \case
    String t -> pure $ AnswerText t
    v@(Number n) ->
      case toBoundedInteger n of
        Nothing -> fail $ "While parsing numeric answer: expected integer, but got: '" ++ show v ++ "'"
        Just i -> pure $ AnswerInt i
    Bool b   -> pure $ AnswerBool b
    v -> fail $ "While parsing answer: expected string, number or boolean, but got '" ++ show v ++ "'"

data QuestionAnswer = QuestionAnswer
  { question :: Text
  , questionSequenceNumber :: Int
  , answer :: Answer
  }
  deriving (Eq, Ord, Read, Show)

data Feedback = Feedback
  { score :: Float
  , content :: [QuestionAnswer]
  }
  deriving (Eq, Ord, Read, Show)

data RoundFeedbacks = RoundFeedbacks
  { round :: Text
  , feedbacks :: NonEmpty Feedback
  }
  deriving (Eq, Ord, Read, Show)

data AdjudicatorFeedbacks = AdjudicatorFeedbacks
  { adjudicator :: Text
  , urlKey :: Text
  , rounds :: NonEmpty RoundFeedbacks
  }
  deriving (Eq, Ord, Read, Show)
