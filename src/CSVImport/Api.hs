module Api
( Token(..)
, getInstitutions
, postInstitutions
, postTeams
, postAdjudicators
) where

import           Data.Aeson (ToJSON,  FromJSON)
import           Data.Foldable (traverse_)
import           Data.Functor (void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Network.HTTP.Simple

import           Api.Types
import qualified Csv

_BASE_URL :: String
_BASE_URL = "https://tabbycat.limperg.de/api/v1"

newtype Token = Token { fromToken :: Text }

type TournamentSlug = String

setToken :: Token -> Request -> Request
setToken (Token tk)
  = setRequestHeader "Authorization" ["Token " <> T.encodeUtf8 tk]

makeRequest_ :: Token -> String -> IO Request
makeRequest_ token u = setToken token <$> parseRequestThrow (_BASE_URL ++ u)

makeRequest :: (ToJSON a) => Token -> a -> String -> IO Request
makeRequest token body u = setRequestBodyJSON body <$> makeRequest_ token u

get_ :: (FromJSON a) => Token -> String -> IO a
get_ token url = do
  request <- makeRequest_ token url
  getResponseBody <$> httpJSON request

post :: (ToJSON a, FromJSON b) => String -> Token -> a -> IO b
post url token body = do
  request <- setRequestMethod "POST" <$> makeRequest token body url
  getResponseBody <$> httpJSON request

post_ :: (ToJSON a) => String -> Token -> a -> IO ()
post_ url token body = do
  request <- setRequestMethod "POST" <$> makeRequest token body url
  void $ httpNoBody request

getInstitutions :: Token -> IO GetInstitutions
getInstitutions token = get_ token "/institutions"

postInstitution
  :: Token -> AddInstitution -> IO (Csv.InstitutionCode, InstitutionURL)
postInstitution token inst = do
  T.putStrLn $ "Adding institution " <> inst.code
  resp :: AddInstitutionResponse <- post "/institutions" token inst
  pure (Csv.InstitutionCode inst.code, resp.url)

postInstitutions
  :: Token -> [AddInstitution] -> IO (Map Csv.InstitutionCode InstitutionURL)
postInstitutions token insts
  = Map.fromList <$> traverse (postInstitution token) insts

postTeam
  :: Token -> TournamentSlug -> AddTeam -> IO (Csv.TeamReference, TeamURL)
postTeam token tournament team = do
  T.putStrLn $ "Adding team " <> team.reference
  resp :: AddTeamResponse <- post ("/tournaments/" ++ tournament ++ "/teams") token team
  pure
    (Csv.TeamReference team.reference, resp.url)

postTeams
  :: Token -> TournamentSlug -> [AddTeam]
  -> IO (Map Csv.TeamReference TeamURL)
postTeams token tournament teams
  = Map.fromList <$> traverse (postTeam token tournament) teams

postAdjudicator :: Token -> TournamentSlug -> AddAdjudicator -> IO ()
postAdjudicator token tournament adj = do
  T.putStrLn $ "Adding adjudicator " <> adj.name
  post_ ("/tournaments/" ++ tournament ++ "/adjudicators") token adj

postAdjudicators
  :: Token -> TournamentSlug -> [AddAdjudicator]
  -> IO ()
postAdjudicators token tournament = traverse_ (postAdjudicator token tournament)
