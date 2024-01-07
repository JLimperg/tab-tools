module Types where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)

newtype Token = Token { fromToken :: Text }

data Answer
  = AnswerBool Bool
  | AnswerText Text
  | AnswerInt Int
  deriving (Eq, Ord, Read, Show)

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
