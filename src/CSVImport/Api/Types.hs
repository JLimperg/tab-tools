{-# LANGUAGE RecordWildCards #-}

module Api.Types
( InstitutionURL(..)
, TeamURL(..)
, GetInstitutions(..)
, AddInstitution(..)
, AddInstitutionResponse(..)
, AddSpeaker(..)
, AddTeam(..)
, AddTeamResponse(..)
, AddAdjudicator(..)
) where

import qualified Csv
import           Data.Aeson
import           Data.Aeson.Key as Key
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isNothing, catMaybes)
import           Data.Text (Text)
import qualified Data.Vector as Vector

(.=?) :: ToJSON a => Text -> Maybe a -> Maybe (Key, Value)
key .=? val = (Key.fromText key .=) <$> val

object' :: [Maybe (Key, Value)] -> Value
object' = object . catMaybes

newtype InstitutionURL = InstitutionURL
  { fromInstitutionURL :: Text }

instance ToJSON InstitutionURL where
  toJSON url = toJSON url.fromInstitutionURL

instance FromJSON InstitutionURL where
  parseJSON = fmap InstitutionURL . parseJSON

newtype TeamURL = TeamURL
  { fromTeamURL :: Text }

instance ToJSON TeamURL where
  toJSON url = toJSON url.fromTeamURL

instance FromJSON TeamURL where
  parseJSON = fmap TeamURL . parseJSON

newtype GetInstitutions = GetInstitutions
  { institutions :: Map Csv.InstitutionCode InstitutionURL
  }

instance FromJSON GetInstitutions where
  parseJSON = withArray "institutions" $ \a -> do
    codesAndUrls <- Vector.forM a $ \v ->
      flip (withObject "institution") v $ \v -> do
        code <- v .: "code"
        code' <- Csv.InstitutionCode <$> parseJSON code
        url <- v .: "url"
        url' <- parseJSON url
        pure (code', url')
    let m = Map.fromList $ Vector.toList codesAndUrls
    pure $ GetInstitutions m

newtype AddInstitution = AddInstitution
  { code :: Text
  }

instance ToJSON AddInstitution where
  toJSON AddInstitution {..} = object
    [ "name" .= code
    , "code" .= code
    ]

newtype AddInstitutionResponse = AddInstitutionResponse
  { url :: InstitutionURL }

instance FromJSON AddInstitutionResponse where
  parseJSON = withObject "AddInstitutionResponse" $ \v -> AddInstitutionResponse
    <$> v .: "url"

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
  { reference :: Text
  , shortReference :: Text
  , institution :: Maybe InstitutionURL
  , speakers :: [AddSpeaker]
  , institutionConflicts :: [InstitutionURL]
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

newtype AddTeamResponse = AddTeamResponse { url :: TeamURL }

instance FromJSON AddTeamResponse where
  parseJSON = withObject "AddTeamResponse" $ \v -> AddTeamResponse
    <$> v .: "url"

data AddAdjudicator = AddAdjudicator
  { name :: Text
  , email :: Maybe Text
  , phone :: Maybe Text
  , anonymous :: Bool
  , institution :: Maybe InstitutionURL
  , trainee :: Bool
  , institutionConflicts :: [InstitutionURL]
  , teamConflicts :: [TeamURL]
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
