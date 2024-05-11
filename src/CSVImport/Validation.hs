module CSVImport.Validation
( validateSpeakers
, validateAdjudicators )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Api.Types
import CSVImport.Csv qualified as Csv

_MAX_TEAM_REFERENCE_LENGTH :: Int
_MAX_TEAM_REFERENCE_LENGTH = 151

_MAX_SHORT_TEAM_REFERENCE_LENGTH :: Int
_MAX_SHORT_TEAM_REFERENCE_LENGTH = 35

validateSpeakers :: Int {- > 0 -} -> Map TeamReference [Csv.Speaker] -> [String]
validateSpeakers speakersPerTeam speakers =
  concatMap (uncurry validateTeam) $ Map.toList speakers
  where
    validateSpeaker :: Csv.Speaker -> [String]
    validateSpeaker speaker =
      validateTeamReference speaker ++ validateShortTeamReference speaker

    validateTeamReference :: Csv.Speaker -> [String]
    validateTeamReference speaker =
      let ref = speaker.team.name in
      [ "Team name '" ++ T.unpack ref ++ "' is too long (max " ++ show _MAX_TEAM_REFERENCE_LENGTH ++ " characters)"
      | T.length ref > _MAX_TEAM_REFERENCE_LENGTH ]

    validateShortTeamReference :: Csv.Speaker -> [String]
    validateShortTeamReference speaker =
      let ref =
            case speaker.shortTeam of
              Nothing -> speaker.team.name
              Just ref -> ref.fromShortTeamReference in
      [ "Short team name '" ++ T.unpack ref ++ "' is too long (max " ++ show _MAX_SHORT_TEAM_REFERENCE_LENGTH ++ " characters)"
      | T.length ref > _MAX_SHORT_TEAM_REFERENCE_LENGTH ]

    validateTeam :: TeamReference -> [Csv.Speaker] -> [String]
    validateTeam team speakers =
      if length speakers /= speakersPerTeam then
        ["Team '" ++ teamS ++ "' does not have " ++ show speakersPerTeam ++ " speakers"]
      else
        concatMap validateSpeaker speakers ++
        validateShortTeamReferences speakers ++
        validateInstitutions speakers
      where
        teamS :: String
        teamS = T.unpack team.name

        validateShortTeamReferences :: [Csv.Speaker] -> [String]
        validateShortTeamReferences speakers =
          let shortRefs = map (\s -> s.shortTeam) speakers
              shortRef = head shortRefs in
          [ "Members of team '" ++ teamS ++ "' have different short team names"
          | not $ all (== shortRef) shortRefs ]

        validateInstitutions :: [Csv.Speaker] -> [String]
        validateInstitutions speakers =
          let insts = map (\s -> s.institution) speakers
              inst = head insts in
          [ "Members of team '" ++ teamS ++ "' have different institutions"
          | not $ all (== inst) insts ]

validateAdjudicators :: [Csv.Adjudicator] -> [String]
validateAdjudicators = const []
