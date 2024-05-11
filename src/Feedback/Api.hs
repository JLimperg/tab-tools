{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Feedback.Api where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Req
import Text.URI (URI(..), mkURI, mkScheme)

import Feedback.Types (Token(..), Answer)

data QuestionAnswer = QuestionAnswer
  { question :: Text
  , answer :: Answer
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON QuestionAnswer

data FeedbackReq = FeedbackReq
  { adjudicator :: Text
  , debate :: Text
  , answers :: [QuestionAnswer]
  , confirmed :: Bool
  , score :: Float
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON FeedbackReq

newtype RoundReq = RoundReq
  { name :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON RoundReq

data AdjudicatorReq = AdjudicatorReq
  { id :: Int
  , name :: Text
  , url_key :: Text
  , email :: Maybe Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON AdjudicatorReq

data QuestionReq = QuestionReq
  { text :: Text
  , seq :: Int
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON QuestionReq

authHeader :: Token -> Option scheme
authHeader (Token tk) = header "Authorization" $ "Token " <> Text.encodeUtf8 tk

getOpts :: FromJSON a => Url 'Https -> Option 'Https -> Token -> IO a
getOpts url opts token = runReq defaultHttpConfig $
  responseBody <$> req GET url NoReqBody jsonResponse
    (authHeader token <> opts)

getParseURL :: FromJSON a => Text -> Token -> IO a
getParseURL url token = do
  httpsScheme <- mkScheme "https"
  uri <- mkURI url
  let uri' = uri { uriScheme = Just httpsScheme }
  case useHttpsURI uri' of
    Just (url, opts) -> getOpts url opts token
    Nothing -> fail $ "Invalid URL: " ++ Text.unpack url

get :: FromJSON a => Url 'Https -> Token -> IO a
get url = getOpts url mempty

getFeedback :: Url 'Https -> Token -> IO [FeedbackReq]
getFeedback baseUrl = get $ baseUrl /: "feedback"

getRound :: Text -> Token -> IO RoundReq
getRound = getParseURL

getAdjudicator :: Text -> Token -> IO AdjudicatorReq
getAdjudicator = getParseURL

getQuestion :: Text -> Token -> IO QuestionReq
getQuestion = getParseURL
