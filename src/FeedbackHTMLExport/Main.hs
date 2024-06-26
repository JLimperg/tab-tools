module FeedbackHTMLExport.Main (main) where

import Data.ByteString.Lazy qualified as BS
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.IO qualified as Text

import Api
import FeedbackHTMLExport.CmdArgs (CmdArgs(..))
import FeedbackHTMLExport.Render (RenderOptions (..), render)
import FeedbackHTMLExport.RenderEmailTable (Adjudicator(..), renderEmailTable)
import FeedbackHTMLExport.Types as Types

data RawFeedback = RawFeedback
  { adjudicatorId :: Int
  , adjudicatorName :: Text
  , adjudicatorEmail :: Maybe Text
  , urlKey :: Text
  , roundId :: Int
  , roundName :: Text
  , feedback :: Feedback
  }

mungeAnswer :: [Text] -> Api.QuestionAnswer -> ApiM (Maybe Types.QuestionAnswer)
mungeAnswer hiddenQuestions Api.QuestionAnswer { question, answer } = do
  QuestionReq { text = questionText, seq } <-
    getQuestion question
  if questionText `elem` hiddenQuestions
    then pure Nothing
    else pure $ Just $ Types.QuestionAnswer
          { question = questionText
          , questionSequenceNumber = seq
          , answer = answer
          }

mungeFeedback
  :: [Text]
  -> FeedbackReq
  -> ApiM (Maybe RawFeedback)
mungeFeedback _ FeedbackReq { confirmed = False } = pure Nothing
mungeFeedback hiddenQuestions FeedbackReq { adjudicator, debate, answers, score }
  = do
    AdjudicatorReq { id = adjId, name = adjName, url_key, email = adjEmail } <-
      getAdjudicator adjudicator
    content <-
      sortOn (\answer -> answer.questionSequenceNumber) . catMaybes <$>
        traverse (mungeAnswer hiddenQuestions) answers
    let feedback = Feedback { score, content }
    (roundId, roundURL) <- debateURLToRound debate
    RoundReq { name = roundName } <- getRound roundURL
    pure $ Just $ RawFeedback
      { adjudicatorId = adjId
      , adjudicatorName = adjName
      , adjudicatorEmail = adjEmail
      , urlKey = url_key
      , roundId
      , roundName
      , feedback
      }

index :: (Foldable f) => (a -> Int) -> f a -> IntMap (NonEmpty a)
index f = foldr (\a m -> IntMap.insertWith (<>) (f a) (a :| []) m) IntMap.empty

mungeFeedbacks :: [RawFeedback] -> [AdjudicatorFeedbacks]
mungeFeedbacks feedbacks
  = toList
  $ IntMap.map mungeAdjudicatorFeedbacks
  $ index (\fb -> fb.adjudicatorId) feedbacks
  where
    mungeAdjudicatorFeedbacks
      :: NonEmpty RawFeedback
      -> AdjudicatorFeedbacks
    mungeAdjudicatorFeedbacks feedbacks
      = let RawFeedback { adjudicatorName, urlKey } = NonEmpty.head feedbacks
            feedbackByRound
              = NonEmpty.groupAllWith1 (\fb -> fb.roundId) feedbacks in
        AdjudicatorFeedbacks
          { adjudicator = adjudicatorName
          , urlKey = urlKey
          , rounds = fmap mungeRoundFeedbacks feedbackByRound
          }

    mungeRoundFeedbacks :: NonEmpty RawFeedback -> RoundFeedbacks
    mungeRoundFeedbacks feedbacks
      = let RawFeedback { roundName } = NonEmpty.head feedbacks in
        RoundFeedbacks
          { round = roundName
          , feedbacks = fmap (\fb -> fb.feedback) feedbacks
          }

mungeAdjudicators :: [RawFeedback] -> [Adjudicator]
mungeAdjudicators =
  nubOrdOn (\ Adjudicator { urlKey } -> urlKey) .
  map rawFeedbackToAdjudicator
  where
    rawFeedbackToAdjudicator :: RawFeedback -> Adjudicator
    rawFeedbackToAdjudicator RawFeedback
        { urlKey, adjudicatorName, adjudicatorEmail }
      = Adjudicator { urlKey, email = adjudicatorEmail, name = adjudicatorName }

validateAdjudicators :: [Adjudicator] -> IO ()
validateAdjudicators = mapM_ $ \ Adjudicator { email, name } ->
  case email of
    Nothing -> Text.putStrLn $
      "Warning: no email address for adjudicator " <> name
    Just _ -> pure ()

main :: CmdArgs -> IO ()
main args = do
  rawFeedbacks <- runApiM args.tabbycatToken args.tabbycatInstance $ do
    feedbackReqs <- getFeedback
    catMaybes <$> traverse (mungeFeedback args.hiddenQuestions) feedbackReqs
  let feedbacks = mungeFeedbacks rawFeedbacks
  let renderOptions = RenderOptions args.baseDir args.randomizeOrder
  render renderOptions feedbacks
  let adjudicators = mungeAdjudicators rawFeedbacks
  validateAdjudicators adjudicators
  let emailTable = renderEmailTable adjudicators
  let emailTableFile = args.emailTableFile
  BS.writeFile emailTableFile emailTable
