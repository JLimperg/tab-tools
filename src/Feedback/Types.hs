{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Feedback.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Api.Types (FeedbackAnswer)

data QuestionAnswer = QuestionAnswer
  { question :: Text
  , questionSequenceNumber :: Int
  , answer :: FeedbackAnswer
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
