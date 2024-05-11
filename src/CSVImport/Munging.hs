{-# LANGUAGE RecordWildCards #-}

module CSVImport.Munging
( mungeTeams
, mungeAdjudicators
, mungeInstitutions
) where

import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T

import CSVImport.Csv qualified as Csv
import Api
import Api.Types as Api

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

lookupInstitutionURL
  :: Map InstitutionCode Link
  -> InstitutionCode
  -> Either String Link
lookupInstitutionURL m i
  = let err = "institution without associated URL: " ++ T.unpack i.code
    in maybeToEither err $ Map.lookup i m

lookupTeamURL
  :: Map TeamReference Link
  -> TeamReference
  -> Either String Link
lookupTeamURL m t
  = let err = "team without associated URL: " ++ T.unpack t.name
    in maybeToEither err $ Map.lookup t m

mungeSpeaker :: Csv.Speaker -> Api.AddSpeaker
mungeSpeaker Csv.Speaker {..} = Api.AddSpeaker
  { name = coerce name
  , email = coerce email
  , phone = coerce phone
  , anonymous = anonymous
  }

mungeTeam
  :: Map InstitutionCode Link
  -> TeamReference
  -> [Csv.Speaker]
  -> Either String Api.AddTeam
mungeTeam institutionURLs reference speakers = do
  let s = head speakers -- TODO use NonEmpty
  let institution = s.institution
  let referenceT = coerce reference
  let shortReference = maybe referenceT coerce s.shortTeam
  instURL <- traverse (lookupInstitutionURL institutionURLs) institution
  instConflicts <-
    traverse (lookupInstitutionURL institutionURLs) $
      concatMap (\s -> s.institutionConflicts) speakers
  pure $ Api.AddTeam
    { reference = referenceT
    , shortReference = coerce shortReference
    , institution = instURL
    , speakers = map mungeSpeaker speakers
    , institutionConflicts = instConflicts
    }

mungeTeams
  :: Map InstitutionCode Link
  -> Map TeamReference [Csv.Speaker]
  -> Either String [Api.AddTeam]
mungeTeams institutionURLs speakers
  = traverse (uncurry $ mungeTeam institutionURLs) (Map.toList speakers)

mungeAdjudicator
  :: Map InstitutionCode Link
  -> Map TeamReference Link
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
  :: Map InstitutionCode Link
  -> Map TeamReference Link
  -> [Csv.Adjudicator]
  -> Either String [Api.AddAdjudicator]
mungeAdjudicators institutionURLs teamURLs
  = traverse (mungeAdjudicator institutionURLs teamURLs)

mungeInstitution :: InstitutionCode -> Api.AddInstitution
mungeInstitution (InstitutionCode c) = Api.AddInstitution $ Api.InstitutionCode c

speakerInstitutions :: Csv.Speaker -> [InstitutionCode]
speakerInstitutions speaker
  = maybeToList speaker.institution ++ speaker.institutionConflicts

adjudicatorInstitutions :: Csv.Adjudicator -> [InstitutionCode]
adjudicatorInstitutions adj
  = maybeToList adj.institution ++ adj.institutionConflicts

mungeInstitutions
  :: Map TeamReference [Csv.Speaker]
  -> [Csv.Adjudicator]
  -> [Api.AddInstitution]
mungeInstitutions teams adjs
  = map mungeInstitution $ toList $ Set.fromList $
      concatMap speakerInstitutions (concat $ toList teams) ++
      concatMap adjudicatorInstitutions adjs
