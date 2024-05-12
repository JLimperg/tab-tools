module CSVImport.Main (main) where

import Control.Monad (unless, filterM)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import System.Exit (exitFailure)

import Api
import Api.Types
import CSVImport.CmdArgs
import CSVImport.Csv as Csv
import CSVImport.Munging
import CSVImport.Validation

_SPEAKERS_PER_TEAM :: Int
_SPEAKERS_PER_TEAM = 2

eitherToMonadFail :: MonadFail m => Either String a -> m a
eitherToMonadFail (Left err) = fail err
eitherToMonadFail (Right a) = pure a

main :: CmdArgs -> IO ()
main args = do
  speakers <- parseSpeakers args.speakersFile
  adjsVec <- parseAdjudicators args.adjudicatorsFile
  let adjs = toList adjsVec
  let numAdjs = Vector.length adjsVec

  let errors =
        validateSpeakers _SPEAKERS_PER_TEAM speakers ++
        validateAdjudicators adjs
  unless (null errors) $ do
    putStrLn "Errors in participant data:"
    mapM_ putStrLn errors
    exitFailure

  let insts = mungeInstitutions speakers adjs
  runApiM args.tabbycatToken args.tabbycatInstance $ do
    GetInstitutions { institutions = oldInstitutions } <- getInstitutions
    insts <- flip filterM insts $ \AddInstitution { code = code } -> do
      if code `Map.member` oldInstitutions then do
        lift $ putStrLn $ "Skipping existing institution " ++ Text.unpack code.code
        pure False
      else
        pure True
    newInstitutions <- postInstitutions insts
    lift $ putStrLn $ "Added " ++ show (length newInstitutions) ++ " institutions."
    let allInstitutions = Map.union oldInstitutions newInstitutions
    teams' <- eitherToMonadFail $ mungeTeams allInstitutions speakers
    teamMap <- postTeams teams'
    lift $ putStrLn $ "Added " ++ show (length teamMap) ++ " teams."
    adjs' <- eitherToMonadFail $ mungeAdjudicators allInstitutions teamMap adjs
    postAdjudicators adjs'
    lift $ putStrLn $ "Added " ++ show numAdjs ++ " adjudicators."
