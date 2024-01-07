{-# LANGUAGE RecordWildCards #-}

module Munging
( mungeTeams
, mungeAdjudicators
, mungeInstitutions
) where

import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Csv
import qualified Api.Types as Api

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

lookupInstitutionURL
  :: Map Csv.InstitutionCode Api.InstitutionURL
  -> Csv.InstitutionCode
  -> Either String Api.InstitutionURL
lookupInstitutionURL m i
  = let err =
          "institution without associated URL: " ++
          T.unpack i.fromInstitutionCode
    in maybeToEither err $ Map.lookup i m

lookupTeamURL
  :: Map Csv.TeamReference Api.TeamURL
  -> Csv.TeamReference
  -> Either String Api.TeamURL
lookupTeamURL m t
  = let err =
          "team without associated URL: " ++
          T.unpack t.fromTeamReference
    in maybeToEither err $ Map.lookup t m

mungeSpeaker :: Csv.Speaker -> Api.AddSpeaker
mungeSpeaker Csv.Speaker {..} = Api.AddSpeaker
  { name = coerce name
  , email = coerce email
  , phone = coerce phone
  , anonymous = anonymous
  }

mungeTeam
  :: Map Csv.InstitutionCode Api.InstitutionURL
  -> Csv.TeamReference
  -> [Csv.Speaker]
  -> Either String Api.AddTeam
mungeTeam institutionURLs reference speakers = do
  let s = head speakers
  let institution = s.institution
  let referenceT = coerce reference
  let shortReference = maybe referenceT coerce s.shortTeam
  instURL <- traverse (lookupInstitutionURL institutionURLs) institution
  instConflicts <-
    traverse (lookupInstitutionURL institutionURLs) $
      concatMap (\s -> s.institutionConflicts) speakers
  pure $ Api.AddTeam
    { reference = referenceT
    , shortReference = shortReference
    , institution = instURL
    , speakers = map mungeSpeaker speakers
    , institutionConflicts = instConflicts
    }

mungeTeams
  :: Map Csv.InstitutionCode Api.InstitutionURL
  -> Map Csv.TeamReference [Csv.Speaker]
  -> Either String [Api.AddTeam]
mungeTeams institutionURLs speakers
  = traverse (uncurry $ mungeTeam institutionURLs) (Map.toList speakers)

mungeAdjudicator
  :: Map Csv.InstitutionCode Api.InstitutionURL
  -> Map Csv.TeamReference Api.TeamURL
  -> Csv.Adjudicator
  -> Either String Api.AddAdjudicator
mungeAdjudicator institutionURLs teamURLs Csv.Adjudicator {..} = do
  instURL <- traverse (lookupInstitutionURL institutionURLs) institution
  instConflicts <-
    traverse (lookupInstitutionURL institutionURLs) institutionConflicts
  teamConflicts' <-
    traverse (lookupTeamURL teamURLs) teamConflicts
  pure $ Api.AddAdjudicator
    { name = coerce name
    , email = coerce email
    , phone = coerce phone
    , anonymous = anonymous
    , institution = instURL
    , trainee = trainee
    , institutionConflicts = instConflicts
    , teamConflicts = teamConflicts'
    }

mungeAdjudicators
  :: Map Csv.InstitutionCode Api.InstitutionURL
  -> Map Csv.TeamReference Api.TeamURL
  -> [Csv.Adjudicator]
  -> Either String [Api.AddAdjudicator]
mungeAdjudicators institutionURLs teamURLs
  = traverse (mungeAdjudicator institutionURLs teamURLs)

mungeInstitution :: Csv.InstitutionCode -> Api.AddInstitution
mungeInstitution (Csv.InstitutionCode c) = Api.AddInstitution c

speakerInstitutions :: Csv.Speaker -> [Csv.InstitutionCode]
speakerInstitutions speaker
  = maybeToList speaker.institution ++ speaker.institutionConflicts

adjudicatorInstitutions :: Csv.Adjudicator -> [Csv.InstitutionCode]
adjudicatorInstitutions adj
  = maybeToList adj.institution ++ adj.institutionConflicts

mungeInstitutions
  :: Map Csv.TeamReference [Csv.Speaker]
  -> [Csv.Adjudicator]
  -> [Api.AddInstitution]
mungeInstitutions teams adjs
  = map mungeInstitution $ toList $ Set.fromList $
      concatMap speakerInstitutions (concat $ toList teams) ++
      concatMap adjudicatorInstitutions adjs
