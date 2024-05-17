{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# LANGUAGE OverloadedLists #-}

module FeedbackCSVExport.Main (main) where

import Data.Csv
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Api
import Api.Types (renderFeedbackAnswer, FeedbackAnswer)
import FeedbackCSVExport.CmdArgs

data Feedbacks = Feedbacks
  { headers :: Vector BS.ByteString
  , feedbacks :: [NamedRecord]
  }

renderFeedbacks :: Feedbacks -> BL.ByteString
renderFeedbacks fb = encodeByName fb.headers fb.feedbacks

data QuestionAnswer' = QuestionAnswer'
  { questionReference :: Text
  , questionSequenceNumber :: Int
  , answer :: FeedbackAnswer
  }

data Feedback = Feedback
  { id :: Int
  , roundSeq :: Int
  , adjudicator :: Text
  , source :: FeedbackSourceReq
  , score :: Float
  , confirmed :: Bool
  , ignored :: Bool
  , answers :: [QuestionAnswer']
  }

completeAnswers :: [QuestionAnswer] -> ApiM [QuestionAnswer']
completeAnswers = mapM go
  where
    go :: QuestionAnswer -> ApiM QuestionAnswer'
    go q = do
      question <- getQuestion q.question
      pure $ QuestionAnswer'
        { questionReference = question.reference
        , questionSequenceNumber = question.seq
        , answer = q.answer
        }

completeFeedbacks :: [FeedbackReq] -> ApiM [Feedback]
completeFeedbacks = mapM go
  where
    go :: FeedbackReq -> ApiM Feedback
    go fb = do
      adjudicator <- getAdjudicator fb.adjudicator
      (_, roundURL) <- debateURLToRound fb.debate
      round <- getRound roundURL
      source <- getFeedbackSource fb.source
      answers <- completeAnswers fb.answers
      pure $ Feedback
        { id = fb.id
        , roundSeq = round.seq
        , adjudicator = adjudicator.name
        , source
        , score = fb.score
        , confirmed = fb.confirmed
        , ignored = fb.ignored
        , answers
        }

collectQuestionNames :: [Feedback] -> [BS.ByteString]
collectQuestionNames fbs = map snd $ Map.toAscList questionMap
  where
    questionMap :: Map Int BS.ByteString
    questionMap = foldr
      (\fb m ->
        foldr
          (\q -> Map.insert q.questionSequenceNumber $
            Text.encodeUtf8 q.questionReference)
          m fb.answers)
      mempty fbs

boolToField :: Bool -> Field
boolToField = \case
  False -> "0"
  True -> "1"

feedbackToNamedRecord :: HashSet BS.ByteString -> Feedback -> NamedRecord
feedbackToNamedRecord questionNames fb
  = baseMap <> answerMap <> missingAnswerMap
  where
    baseMap =
      [ ("id", toField fb.id)
      , ("round", toField fb.roundSeq)
      , ("adjudicator", toField fb.adjudicator)
      , ("source", toField $ feedbackSourceIdent fb.source)
      , ("score", toField fb.score)
      , ("confirmed", boolToField fb.confirmed)
      , ("ignored", boolToField fb.ignored)
      ]

    answerMap = HashMap.fromList
      [ (toField qa.questionReference, toField $ renderFeedbackAnswer qa.answer)
      | qa <- fb.answers ]

    missingAnswerMap =
      let missingQuestions
            = HashSet.difference questionNames (HashMap.keysSet answerMap) in
      HashSet.foldl' (\m q -> HashMap.insert q "" m) mempty missingQuestions

nonQuestionHeaders :: Vector BS.ByteString
nonQuestionHeaders =
  ["id", "round", "adjudicator", "source", "score", "confirmed", "ignored"]

newtype FeedbackId = FeedbackId { id :: Int }
  deriving (Generic)

instance FromNamedRecord FeedbackId

getAlreadyExportedFeedbacks :: Maybe FilePath -> IO (Maybe IntSet)
getAlreadyExportedFeedbacks csvFileM = do
  case csvFileM of
    Nothing -> pure Nothing
    Just csvFile -> do
      input <- BL.readFile csvFile
      let result :: Either String (Header, Vector FeedbackId)
            = decodeByName input
      case result of
        Left err -> fail err
        Right (_, ids) ->
          pure $ Just $ IntSet.fromList $ map (\x -> x.id) $ Vector.toList ids

mungeFeedbacks
  :: Bool -- Omit ignored feedbacks
  -> Bool -- Omit unconfirmed feedbacks
  -> Maybe IntSet -- Omit feedbacks with these IDs
  -> [FeedbackReq]
  -> ApiM Feedbacks
mungeFeedbacks omitIgnored omitUnconfirmed oldFeedbacksM fbs = do
  let fbs'
        = case oldFeedbacksM of
            Nothing -> fbs
            Just oldFeedbacks ->
              filter (\fb -> not $ fb.id `IntSet.member` oldFeedbacks) fbs
      fbs''
        = if not (omitIgnored || omitUnconfirmed) then
            fbs'
          else
            filter
              (\fb -> not (omitIgnored && fb.ignored) &&
                      not (omitUnconfirmed && not fb.confirmed))
              fbs'
  fbs''' <- completeFeedbacks fbs''
  let questionNames = collectQuestionNames fbs'''
      headers = nonQuestionHeaders <> Vector.fromList questionNames
      questionNameSet = HashSet.fromList questionNames
      feedbacks = map (feedbackToNamedRecord questionNameSet) fbs'''
  pure Feedbacks { headers, feedbacks }

main :: CmdArgs -> IO ()
main args = do
  oldFeedbacksM <- getAlreadyExportedFeedbacks args.oldFeedbackFile
  fbs <- runApiM args.token args.tabbycatInstance $
    getFeedback >>=
      mungeFeedbacks args.omitIgnored args.omitUnconfirmed oldFeedbacksM
  let fbsRendered = renderFeedbacks fbs
  createDirectoryIfMissing True $ takeDirectory args.outputFile
  BL.writeFile args.outputFile fbsRendered
