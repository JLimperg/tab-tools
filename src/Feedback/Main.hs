module Main (main) where

import Data.ByteString.Lazy qualified as BS
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Read (readMaybe)

import Api
import Api.Types (Link(..))
import Feedback.CmdArgs (CmdArgs(..), parseCmdArgs)
import Feedback.Render (RenderOptions (..), render)
import Feedback.RenderEmailTable (Adjudicator(..), renderEmailTable)
import Feedback.Types as Types

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
    let roundUrlParts = drop 2 $ reverse (Text.splitOn "/" debate.url)
    let roundId = fromJust $ readMaybe . Text.unpack =<< listToMaybe roundUrlParts -- TODO error handling
    let roundUrl = Link $ Text.intercalate "/" $ reverse roundUrlParts
    RoundReq { name = roundName} <- getRound roundUrl
    pure $ Just $ RawFeedback
      { adjudicatorId = adjId
      , adjudicatorName = adjName
      , adjudicatorEmail = adjEmail
      , urlKey = url_key
      , roundId = roundId
      , roundName = roundName
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

main :: IO ()
main = do
  args <- parseCmdArgs
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
