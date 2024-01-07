module Api
( ParticipantReq(..)
, getSpeakers
, getAdjudicators )
where

import           Data.Aeson (FromJSON)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           GHC.Generics (Generic)
import           Network.HTTP.Req

import           Types (Token(..), TabbycatInstance(..))

data ParticipantReq = ParticipantReq
  { name :: Text
  , url_key :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON ParticipantReq

apiBaseUrl :: TabbycatInstance -> Url 'Https
apiBaseUrl TabbycatInstance { host, tournament } =
  https host /: "api" /: "v1" /: "tournaments" /: tournament

authHeader :: Token -> Option scheme
authHeader (Token tk) = header "Authorization" $
  "Token " <> Text.encodeUtf8 tk

getOpts :: FromJSON a => Url 'Https -> Option 'Https -> Token -> IO a
getOpts url opts token = runReq defaultHttpConfig $
  responseBody <$> req GET url NoReqBody jsonResponse
    (authHeader token <> opts)

get :: FromJSON a => Url 'Https -> Token -> IO a
get url = getOpts url mempty

getSpeakers :: TabbycatInstance -> Token -> IO [ParticipantReq]
getSpeakers tabbycat = get $ apiBaseUrl tabbycat /: "speakers"

getAdjudicators :: TabbycatInstance -> Token -> IO [ParticipantReq]
getAdjudicators tabbycat = get $ apiBaseUrl tabbycat /: "adjudicators"
