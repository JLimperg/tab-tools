{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Render (RenderOptions(..), render) where

import           Prelude hiding (head, div)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory (createDirectoryIfMissing)
import           System.Random (StdGen, initStdGen)
import           System.Random.Shuffle (shuffle')
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes as Html
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import           Types
import           Static (stylesheet)

data RenderOptions = RenderOptions
  { baseDir :: FilePath
  , randomizeOrder :: Bool
  }

render :: RenderOptions -> [AdjudicatorFeedbacks] -> IO ()
render opts@RenderOptions { baseDir } feedbacks = do
  createDirectoryIfMissing True baseDir
  mapM_ (writeFeedbacks opts) feedbacks
  writeStylesheet opts

writeStylesheet :: RenderOptions -> IO ()
writeStylesheet RenderOptions { baseDir } =
  BS.writeFile (baseDir ++ "/style.css") stylesheet

writeFeedbacks :: RenderOptions -> AdjudicatorFeedbacks -> IO ()
writeFeedbacks opts@RenderOptions { baseDir } fb@AdjudicatorFeedbacks { urlKey }
  = do
    let filename = baseDir ++ "/" ++ Text.unpack urlKey ++ ".html"
    gen <- initStdGen
    BSL.writeFile filename $ renderHtml $ renderFeedbacks gen opts fb

renderFeedbacks :: StdGen -> RenderOptions -> AdjudicatorFeedbacks -> Html
renderFeedbacks gen RenderOptions { randomizeOrder = True }
  AdjudicatorFeedbacks { adjudicator, rounds }
  = withSiteTemplate adjudicator $ do
      p "Each box is one feedback sheet. The sheets appear in random order."
      mapM_ renderFeedback $ randomizeFeedbacks gen rounds
renderFeedbacks _ RenderOptions { randomizeOrder = False }
  AdjudicatorFeedbacks { adjudicator, rounds }
  = withSiteTemplate adjudicator $ mapM_ renderRound rounds

withSiteTemplate :: Text -> Html -> Html
withSiteTemplate adjudicator rest = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    link ! rel "stylesheet" ! href "style.css"
  body $ do
    h1 $ text $ "Feedback for " <> adjudicator
    rest

randomizeFeedbacks :: StdGen -> NonEmpty RoundFeedbacks -> [Feedback]
randomizeFeedbacks gen rounds =
  let fbs = concatMap (NE.toList . feedbacks) $ NE.toList rounds in
  shuffle' fbs (length fbs) gen

renderRound :: RoundFeedbacks -> Html
renderRound RoundFeedbacks { round, feedbacks } = do
  h2 $ text round
  mapM_ renderFeedback feedbacks

renderFeedback :: Feedback -> Html
renderFeedback Feedback { score, content } = do
  div ! class_ "feedback" $ do
    p ! class_ "question" $ "Overall score"
    p ! class_ "answer"  $ string $ show score
    mapM_ renderQuestionAnswer content

renderQuestionAnswer :: QuestionAnswer -> Html
renderQuestionAnswer QuestionAnswer { question, answer } = do
  p ! class_ "question" $ text question
  p ! class_ "answer" $ renderAnswer answer

renderAnswer :: Answer -> Html
renderAnswer = \case
  AnswerBool True -> "yes"
  AnswerBool False -> "no"
  AnswerText t -> mapM_ (p . text) $ Text.lines t
  AnswerInt i -> string $ show i
