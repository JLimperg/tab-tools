module Main (main) where

import           Control.Monad (unless, filterM)
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           System.Exit (exitFailure)

import           Api
import           Api.Types (GetInstitutions(..), AddInstitution (..))
import           Csv
import           Munging
import           Validation

_SPEAKER_CSV, _ADJ_CSV :: FilePath
_SPEAKER_CSV = "speakers.csv"
_ADJ_CSV = "adjudicators.csv"

_SPEAKERS_PER_TEAM :: Int
_SPEAKERS_PER_TEAM = 2

_TOKEN :: Token
_TOKEN = Token "52298698979d985f1c36df97dd545d338875cffe"

_TOURNAMENT_SLUG :: String
_TOURNAMENT_SLUG = "zdj2023"

eitherToMonadFail :: MonadFail m => Either String a -> m a
eitherToMonadFail (Left err) = fail err
eitherToMonadFail (Right a) = pure a

main :: IO ()
main = do
  speakers <- parseSpeakers _SPEAKER_CSV
  adjs <- toList <$> parseAdjudicators _ADJ_CSV

  let errors =
        validateSpeakers _SPEAKERS_PER_TEAM speakers ++
        validateAdjudicators adjs
  unless (null errors) $ do
    putStrLn "Errors in participant data:"
    mapM_ putStrLn errors
    exitFailure

  let insts = mungeInstitutions speakers adjs
  existingInstMap <- (\resp -> resp.institutions) <$> getInstitutions _TOKEN
  insts <- flip filterM insts $ \AddInstitution { code = code } -> do
    if coerce code `Map.member` existingInstMap then do
      putStrLn $ "Skipping existing institution " ++ Text.unpack code
      pure False
    else
      pure True
  instMap' <- postInstitutions _TOKEN insts
  let instMap = Map.union instMap' existingInstMap
  putStrLn $ "Added " ++ show (length instMap) ++ " institutions."
  teams' <- eitherToMonadFail $ mungeTeams instMap speakers
  teamMap <- postTeams _TOKEN _TOURNAMENT_SLUG teams'
  putStrLn $ "Added " ++ show (length teamMap) ++ " teams."
  adjs' <- eitherToMonadFail $ mungeAdjudicators instMap teamMap adjs
  postAdjudicators _TOKEN _TOURNAMENT_SLUG adjs'
  putStrLn $ "Added " ++ show (length adjs) ++ " adjudicators."
