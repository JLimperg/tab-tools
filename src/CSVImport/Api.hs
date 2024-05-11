module CSVImport.Api
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Network.HTTP.Req

import           CSVImport.Api.Types
import qualified CSVImport.Csv as Csv

-- TODO evil hardcoding
_BASE_URL :: String
_BASE_URL = "https://tabbycat.limperg.de/api/v1"

newtype Token = Token { fromToken :: Text }

type TournamentSlug = String

authHeader :: Token -> Option scheme
authHeader (Token tk) = header "Authorization" $ "Token " <> T.encodeUtf8 tk

mkUrl :: [Text] -> Url 'Https
mkUrl = foldl (/:) (https "tabbycat.limperg.de" /: "api" /: "v1")

getOpts :: FromJSON a => Url 'Https -> Option 'Https -> Token -> IO a
getOpts url opts token = runReq defaultHttpConfig $
  responseBody <$> req GET url NoReqBody jsonResponse
    (authHeader token <> opts)

get :: FromJSON a => Url 'Https -> Token -> IO a
get url = getOpts url mempty

postOpts ::
  (ToJSON a, FromJSON b) => Url 'Https -> Option 'Https -> Token -> a -> IO b
postOpts url opts token body = runReq defaultHttpConfig $
  responseBody <$> req POST url (ReqBodyJson body) jsonResponse
    (authHeader token <> opts)

post :: (ToJSON a, FromJSON b) => Url 'Https -> Token -> a -> IO b
post url = postOpts url mempty

postOpts_ :: ToJSON a => Url 'Https -> Option 'Https -> Token -> a -> IO ()
postOpts_ url opts token body = void $ runReq defaultHttpConfig $
  req POST url (ReqBodyJson body) ignoreResponse
    (authHeader token <> opts)

post_ :: ToJSON a => Url 'Https -> Token -> a -> IO ()
post_ url = postOpts_ url mempty

getInstitutions :: Token -> IO GetInstitutions
getInstitutions = get (mkUrl ["institutions"])

postInstitution
  :: Token -> AddInstitution -> IO (Csv.InstitutionCode, InstitutionURL)
postInstitution token inst = do
  T.putStrLn $ "Adding institution " <> inst.code
  resp :: AddInstitutionResponse <- post (mkUrl ["institutions"]) token inst
  pure (Csv.InstitutionCode inst.code, resp.url)

postInstitutions
  :: Token -> [AddInstitution] -> IO (Map Csv.InstitutionCode InstitutionURL)
postInstitutions token insts
  = Map.fromList <$> traverse (postInstitution token) insts

postTeam
  :: Token -> TournamentSlug -> AddTeam -> IO (Csv.TeamReference, TeamURL)
postTeam token tournament team = do
  T.putStrLn $ "Adding team " <> team.reference
  resp :: AddTeamResponse <-
    post (mkUrl ["tournaments", T.pack tournament, "teams"]) token team
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
  post_ (mkUrl ["tournaments", T.pack tournament, "adjudicators"]) token adj

postAdjudicators
  :: Token -> TournamentSlug -> [AddAdjudicator]
  -> IO ()
postAdjudicators token tournament = traverse_ (postAdjudicator token tournament)
