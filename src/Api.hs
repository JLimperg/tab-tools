{-# LANGUAGE RecordWildCards #-}

module Api
( ApiM
, runApiM
, getToken
, getTabbycatInstance
, debateURLToRound
, GetParticipantsResponse(..)
, getSpeakers
, getAdjudicators
, QuestionAnswer(..)
, FeedbackReq(..)
, getFeedback
, RoundReq(..)
, getRound
, AdjudicatorReq(..)
, getAdjudicator
, QuestionReq(..)
, getQuestion
, FeedbackSourceReq(..)
, feedbackSourceIdent
, getFeedbackSource
, GetInstitutions(..)
, getInstitutions
, AddInstitution(..)
, postInstitution
, postInstitutions
, AddSpeaker(..)
, AddTeam(..)
, postTeam
, postTeams
, AddAdjudicator(..)
, postAdjudicator
, postAdjudicators
) where

import Control.Applicative ((<|>))
import Control.Monad (void, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..), Key, (.=), (.:), object, withArray
  , withObject, eitherDecode )
import Data.Aeson.Key qualified as Key
import Data.Foldable (traverse_)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing, catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Network.HTTP.Req
import Text.Read (readMaybe)
import Text.URI (URI(..), mkURI, mkScheme)

import Api.Cache as Cache
import Api.Types

infixr 8 .=?
(.=?) :: ToJSON a => Text -> Maybe a -> Maybe (Key, Value)
key .=? val = (Key.fromText key .=) <$> val

object' :: [Maybe (Key, Value)] -> Value
object' = object . catMaybes

data ApiMContext = ApiMContext
  { token :: Token
  , tabbycatInstance :: TabbycatInstance
  , cacheRef :: IORef (Cache 'Https)
  }

type ApiM = ReaderT ApiMContext IO

runApiM :: Token -> TabbycatInstance -> ApiM a -> IO a
runApiM token inst f = do
  cacheRef <- newIORef Cache.empty
  let ctx = ApiMContext token inst cacheRef
  runReaderT f ctx

getToken :: ApiM Token
getToken = (\c -> c.token) <$> ask

getTabbycatInstance :: ApiM TabbycatInstance
getTabbycatInstance = (\c -> c.tabbycatInstance) <$> ask

getCache :: ApiM (Cache 'Https)
getCache = ask >>= \ctx -> lift $ readIORef ctx.cacheRef

setCache :: Cache 'Https -> ApiM ()
setCache cache = ask >>= \ctx -> lift $ writeIORef ctx.cacheRef cache

baseURL :: TabbycatInstance -> Url 'Https
baseURL TabbycatInstance { host } =
  https host /: "api" /: "v1"

tournamentBaseURL :: TabbycatInstance -> Url 'Https
tournamentBaseURL inst = baseURL inst /: "tournaments" /: inst.tournament

mkURLCore :: (TabbycatInstance -> Url 'Https) -> [Text] -> ApiM (Url 'Https)
mkURLCore mkBase suffix = do
  base <- (\c -> mkBase c.tabbycatInstance) <$> ask
  pure $ foldl' (/:) base suffix

mkURL :: [Text] -> ApiM (Url 'Https)
mkURL = mkURLCore baseURL

mkTournamentURL :: [Text] -> ApiM (Url 'Https)
mkTournamentURL = mkURLCore tournamentBaseURL

authHeader :: Token -> Option scheme
authHeader (Token tk) = header "Authorization" $ "Token " <> Text.encodeUtf8 tk

getOpts :: (FromJSON a) => Url 'Https -> Option 'Https -> ApiM a
getOpts url opts = do
  token <- getToken
  cache <- getCache
  (cache, json) <- lift $ withCache cache url opts $ do
    runReq defaultHttpConfig $ do
      responseBody <$> req GET url NoReqBody lbsResponse
        (authHeader token <> opts)
  setCache cache
  case eitherDecode json of
    Left err -> fail $ "Malformed JSON response: " ++ err
    Right result -> pure result

get :: (FromJSON a) => Url 'Https -> ApiM a
get url = getOpts url mempty

getLink :: (FromJSON a) => Link -> ApiM a
getLink link = do
  httpsScheme <- mkScheme "https"
  uri <- mkURI link.url
  let uri' = uri { uriScheme = Just httpsScheme }
  case useHttpsURI uri' of
    Just (url, opts) -> getOpts url opts
    Nothing -> fail $ "Invalid URL: " ++ Text.unpack link.url

postOpts ::
  (ToJSON a, FromJSON b) => Url 'Https -> Option 'Https -> a -> ApiM b
postOpts url opts body = do
  token <- getToken
  runReq defaultHttpConfig $
    responseBody <$> req POST url (ReqBodyJson body) jsonResponse
      (authHeader token <> opts)

post :: (ToJSON a, FromJSON b) => Url 'Https -> a -> ApiM b
post url = postOpts url mempty

postOpts_ :: ToJSON a => Url 'Https -> Option 'Https -> a -> ApiM ()
postOpts_ url opts body = do
  token <- getToken
  void $ runReq defaultHttpConfig $
    req POST url (ReqBodyJson body) ignoreResponse
      (authHeader token <> opts)

post_ :: ToJSON a => Url 'Https -> a -> ApiM ()
post_ url = postOpts_ url mempty

debateURLToRound :: Link -> ApiM (Int, Link)
debateURLToRound debateURL = do
  let roundURLParts = drop 2 $ reverse (Text.splitOn "/" debateURL.url)
  let roundURL = Link $ Text.intercalate "/" $ reverse roundURLParts
  case readMaybe . Text.unpack =<< listToMaybe roundURLParts of
    Nothing -> fail $ "Unexpected format of round URL: "  ++ show debateURL.url
    Just roundId -> pure (roundId, roundURL)

data GetParticipantsResponse = GetParticipantsResponse
  { name :: Text
  , url_key :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON GetParticipantsResponse

getSpeakers :: ApiM [GetParticipantsResponse]
getSpeakers = mkTournamentURL ["speakers"] >>= get

getAdjudicators :: ApiM [GetParticipantsResponse]
getAdjudicators = mkTournamentURL ["adjudicators"] >>= get

data QuestionAnswer = QuestionAnswer
  { question :: Link
  , answer :: FeedbackAnswer
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON QuestionAnswer

data FeedbackReq = FeedbackReq
  { adjudicator :: Link
  , debate :: Link
  , answers :: [QuestionAnswer]
  , confirmed :: Bool
  , score :: Float
  , source :: Link
  , timestamp :: Text
  , id :: Integer
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON FeedbackReq

getFeedback :: ApiM [FeedbackReq]
getFeedback = mkTournamentURL ["feedback"] >>= get

newtype RoundReq = RoundReq
  { name :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON RoundReq

getRound :: Link -> ApiM RoundReq
getRound = getLink

data AdjudicatorReq = AdjudicatorReq
  { id :: Int
  , name :: Text
  , url_key :: Text
  , email :: Maybe Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON AdjudicatorReq

getAdjudicator :: Link -> ApiM AdjudicatorReq
getAdjudicator = getLink

data QuestionReq = QuestionReq
  { text :: Text
  , seq :: Int
  , name :: Text
  , reference :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON QuestionReq

getQuestion :: Link -> ApiM QuestionReq
getQuestion = getLink

data FeedbackSourceReq
  = FeedbackSourceTeam Text
  | FeedbackSourceAdjudicator Text
  deriving (Read, Show, Eq, Ord)

feedbackSourceIdent :: FeedbackSourceReq -> Text
feedbackSourceIdent = \case
  FeedbackSourceTeam n -> n
  FeedbackSourceAdjudicator n -> n

instance FromJSON FeedbackSourceReq where
  parseJSON v = flip (withObject "FeedbackSource") v $
    \obj -> team obj <|> participant obj
    where
      team obj = do
        ref <- obj .: "short_reference"
        pure $ FeedbackSourceTeam ref

      participant obj = do
        name <- obj .: "name"
        pure $ FeedbackSourceAdjudicator name

getFeedbackSource :: Link -> ApiM FeedbackSourceReq
getFeedbackSource = getLink

newtype GetInstitutions = GetInstitutions
  { institutions :: Map InstitutionCode Link }

instance FromJSON GetInstitutions where
  parseJSON = withArray "institutions" $ \a -> do
    codesAndUrls <- Vector.forM a $ \v ->
      flip (withObject "institution") v $ \v -> do
        code <- v .: "code"
        code' <- InstitutionCode <$> parseJSON code
        url <- v .: "url"
        url' <- parseJSON url
        pure (code', url')
    let m = Map.fromList $ Vector.toList codesAndUrls
    pure $ GetInstitutions m

getInstitutions :: ApiM GetInstitutions
getInstitutions = mkURL ["institutions"] >>= get

newtype AddInstitution = AddInstitution
  { code :: InstitutionCode
  }

instance ToJSON AddInstitution where
  toJSON AddInstitution {..} = object
    [ "name" .= code
    , "code" .= code
    ]

newtype AddInstitutionResponse = AddInstitutionResponse
  { url :: Link }
  deriving (Generic)

instance FromJSON AddInstitutionResponse

postInstitution :: AddInstitution -> ApiM (InstitutionCode, Link)
postInstitution inst = do
  lift $ Text.putStrLn $ "Adding institution " <> inst.code.code
  url <- mkURL ["institutions"]
  resp :: AddInstitutionResponse <- post url inst
  pure (inst.code, resp.url)

postInstitutions :: (Foldable f) =>
  f AddInstitution -> ApiM (Map InstitutionCode Link)
postInstitutions = foldM addInst Map.empty
  where
    addInst ::
      Map InstitutionCode Link ->
      AddInstitution ->
      ApiM (Map InstitutionCode Link)
    addInst m inst = do
      (code, link) <- postInstitution inst
      pure $ Map.insert code link m

data AddSpeaker = AddSpeaker
  { name :: Text
  , email :: Maybe Text
  , phone :: Maybe Text
  , anonymous :: Bool
  }

instance ToJSON AddSpeaker where
  toJSON AddSpeaker {..} = object'
    [ "name" .=? Just name
    , "email" .=? email
    , "phone" .=? phone
    , "anonymous" .=? Just anonymous
    , "categories" .=? Just ([] :: [Value])
    ]

data AddTeam = AddTeam
  { reference :: TeamReference
  , shortReference :: Text
  , institution :: Maybe Link
  , speakers :: [AddSpeaker]
  , institutionConflicts :: [Link]
  }

instance ToJSON AddTeam where
  toJSON AddTeam {..} = object
    [ "reference" .= reference
    , "short_reference" .= shortReference
    , "institution" .= institution
    , "speakers" .= speakers
    , "institution_conflicts" .= institutionConflicts
    , "break_categories" .= Just ([] :: [Value])
    ]

newtype AddTeamResponse = AddTeamResponse { url :: Link }
  deriving (Generic)

instance FromJSON AddTeamResponse

postTeam :: AddTeam -> ApiM (TeamReference, Link)
postTeam team = do
  lift $ Text.putStrLn $ "Adding team " <> team.reference.name
  url <- mkTournamentURL ["teams"]
  resp :: AddTeamResponse <- post url team
  pure (team.reference, resp.url)

postTeams :: [AddTeam] -> ApiM (Map TeamReference Link)
postTeams teams = Map.fromList <$> traverse postTeam teams

data AddAdjudicator = AddAdjudicator
  { name :: Text
  , email :: Maybe Text
  , phone :: Maybe Text
  , anonymous :: Bool
  , institution :: Maybe Link
  , trainee :: Bool
  , institutionConflicts :: [Link]
  , teamConflicts :: [Link]
  }

instance ToJSON AddAdjudicator where
  toJSON AddAdjudicator {..} = object'
    [ "name" .=? Just name
    , "email" .=? email
    , "phone" .=? phone
    , "anonymous" .=? Just anonymous
    , "institution" .=? Just institution
    , "trainee" .=? Just trainee
    , "independent" .=? Just (isNothing institution)
    , "institution_conflicts" .=? Just institutionConflicts
    , "team_conflicts" .=? Just teamConflicts
    , "adjudicator_conflicts" .=? Just ([] :: [Value])
    ]

postAdjudicator :: AddAdjudicator -> ApiM ()
postAdjudicator adj = do
  lift $ Text.putStrLn $ "Adding adjudicator " <> adj.name
  url <- mkTournamentURL ["adjudicators"]
  post_ url adj

postAdjudicators :: [AddAdjudicator] -> ApiM ()
postAdjudicators = traverse_ postAdjudicator
