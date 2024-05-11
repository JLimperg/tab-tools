{-# OPTIONS_GHC -Wno-orphans #-}

module CSVImport.Csv
( Name(..)
, EmailAddress(..)
, PhoneNumber(..)
, ShortTeamReference(..)
, Speaker(..)
, Adjudicator(..)
, parseSpeakers
, parseAdjudicators
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Csv hiding (Name)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)

import Api.Types (InstitutionCode(..), TeamReference(..))

parseBool :: Text -> Parser Bool
parseBool t = case T.strip t of
  "true" -> pure True
  "false" -> pure False
  _ -> fail "expected 'true' or 'false'"

parseText :: Field -> Parser Text
parseText = fmap T.strip . parseField

parseList :: Text -> [Text]
parseList "" = []
parseList t = map T.strip $ T.split (== ';') t

instance FromField InstitutionCode where
  parseField = coerce parseText

instance FromField [InstitutionCode] where
  parseField = fmap (coerce . parseList) . parseText

instance FromField TeamReference where
  parseField = coerce parseText

instance FromField [TeamReference] where
  parseField = fmap (coerce . parseList) . parseField

newtype ShortTeamReference = ShortTeamReference { fromShortTeamReference :: Text }
  deriving (Eq, Ord, Read, Show)

instance FromField ShortTeamReference where
  parseField = coerce parseText

newtype Name = Name { fromName :: Text }
  deriving (Eq, Ord, Read, Show)

instance FromField Name where
    parseField = coerce parseText

newtype EmailAddress = EmailAddress { fromEmailAddress :: Text }
  deriving (Eq, Ord, Read, Show)

instance FromField EmailAddress where
    parseField = coerce parseText

newtype PhoneNumber = PhoneNumber { fromPhoneNumber :: Text }
  deriving (Eq, Ord, Read, Show)

instance FromField PhoneNumber where
    parseField = coerce parseText

data Institution = Institution
  { name :: Text
  , code :: InstitutionCode
  }
  deriving (Read, Show)

instance FromNamedRecord Institution where
  parseNamedRecord r = Institution
    <$> r .: "name"
    <*> r .: "code"

data Speaker = Speaker
  { team :: TeamReference
  , shortTeam :: Maybe ShortTeamReference
  , institution :: Maybe InstitutionCode
  , name :: Name
  , email :: Maybe EmailAddress
  , phone :: Maybe PhoneNumber
  , anonymous :: Bool
  , institutionConflicts :: [InstitutionCode]
  }
  deriving (Read, Show)

instance FromNamedRecord Speaker where
  parseNamedRecord r = Speaker
    <$> r .: "team"
    <*> r .: "short team"
    <*> r .: "institution"
    <*> r .: "name"
    <*> r .: "email"
    <*> r .: "phone"
    <*> (parseBool =<< r .: "anonymous")
    <*> r .: "institution conflicts"

data Adjudicator = Adjudicator
  { name :: Name
  , email :: Maybe EmailAddress
  , phone :: Maybe PhoneNumber
  , anonymous :: Bool
  , institution :: Maybe InstitutionCode
  , trainee :: Bool
  , institutionConflicts :: [InstitutionCode]
  , teamConflicts :: [TeamReference]
  }
  deriving (Read, Show)

instance FromNamedRecord Adjudicator where
  parseNamedRecord r = Adjudicator
    <$> r .: "name"
    <*> r .: "email"
    <*> r .: "phone"
    <*> (parseBool =<< r .: "anonymous")
    <*> r .: "institution"
    <*> (parseBool =<< r .: "trainee")
    <*> r .: "institution conflicts"
    <*> r .: "team conflicts"

parseCsvFile :: (FromNamedRecord a) => FilePath -> IO (Vector a)
parseCsvFile file = do
  input <- BS.readFile file
  let result = decodeByName $ LBS.fromStrict input
  case result of
    (Left err) -> fail err
    (Right (_, r)) -> pure r

insertMultiMap :: (Ord a) => a -> b -> Map a [b] -> Map a [b]
insertMultiMap a b = Map.insertWith (++) a [b]

indexContainer :: (Ord b, Foldable f) => (a -> b) -> f a -> Map b [a]
indexContainer f = foldl' (\m a -> insertMultiMap (f a) a m) Map.empty

parseSpeakers :: FilePath -> IO (Map TeamReference [Speaker])
parseSpeakers file = indexContainer (\s -> s.team) <$> parseCsvFile file

parseAdjudicators :: FilePath -> IO (Vector Adjudicator)
parseAdjudicators = parseCsvFile
