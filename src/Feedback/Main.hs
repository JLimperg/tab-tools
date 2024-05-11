module Main (main) where

import qualified Data.ByteString.Lazy as BS
import           Data.Containers.ListUtils (nubOrdOn)
import           Data.Foldable (toList)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, listToMaybe, catMaybes)
import           Network.HTTP.Req (useHttpsURI)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Text.Read (readMaybe)
import           Text.URI (mkURI)

import           Feedback.Api as Api
import           Feedback.CmdArgs (CmdArgs(..), parseCmdArgs)
import           Feedback.Render (RenderOptions (..), render)
import           Feedback.RenderEmailTable (Adjudicator(..), renderEmailTable)
import           Feedback.Types as Types

data RawFeedback = RawFeedback
  { adjudicatorId :: Int
  , adjudicatorName :: Text
  , adjudicatorEmail :: Maybe Text
  , urlKey :: Text
  , roundId :: Int
  , roundName :: Text
  , feedback :: Feedback
  }

mungeAnswer :: Token -> [Text] -> Api.QuestionAnswer
  -> IO (Maybe Types.QuestionAnswer)
mungeAnswer token hiddenQuestions Api.QuestionAnswer { question, answer } = do
  QuestionReq { text = questionText, seq } <-
    getQuestion question token
  if questionText `elem` hiddenQuestions
    then pure Nothing
    else pure $ Just $ Types.QuestionAnswer
          { question = questionText
          , questionSequenceNumber = seq
          , answer = answer
          }

mungeFeedback
  :: Token
  -> [Text]
  -> FeedbackReq
  -> IO (Maybe RawFeedback)
mungeFeedback _ _ FeedbackReq { confirmed = False } = pure Nothing
mungeFeedback
  token
  hiddenQuestions
  FeedbackReq { adjudicator, debate, answers, score }
  = do
    AdjudicatorReq { id = adjId, name = adjName, url_key, email = adjEmail } <-
      getAdjudicator adjudicator token
    content <-
      sortOn (\answer -> answer.questionSequenceNumber) . catMaybes <$>
        traverse (mungeAnswer token hiddenQuestions) answers
    let feedback = Feedback { score, content }
    let roundUrlParts = drop 2 $ reverse (Text.splitOn "/" debate)
    let roundId = fromJust $ readMaybe . Text.unpack =<< listToMaybe roundUrlParts -- TODO error handling
    let roundUrl = Text.intercalate "/" $ reverse roundUrlParts
    RoundReq { name = roundName} <- getRound roundUrl token
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
  cmdArgs <- parseCmdArgs
  baseURI <- mkURI cmdArgs.baseURL
  baseURL <- case useHttpsURI baseURI of
    Just (url, _) -> pure url
    Nothing -> fail $ "Invalid URL: " ++ Text.unpack cmdArgs.baseURL
  feedbackReqs <- getFeedback baseURL cmdArgs.token
  rawFeedbacks <- catMaybes <$>
    traverse (mungeFeedback cmdArgs.token cmdArgs.hiddenQuestions) feedbackReqs
  let feedbacks = mungeFeedbacks rawFeedbacks
  let renderOptions = RenderOptions
        { baseDir = cmdArgs.baseDir
        , randomizeOrder = cmdArgs.randomizeOrder
        }
  render renderOptions feedbacks
  let adjudicators = mungeAdjudicators rawFeedbacks
  validateAdjudicators adjudicators
  let emailTable = renderEmailTable adjudicators
  let emailTableFile = cmdArgs.emailTableFile
  BS.writeFile emailTableFile emailTable
