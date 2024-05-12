module Feedback.CmdArgs (CmdArgs(..), cmdArgs) where

import Options.Applicative
import Data.Text (Text)

import Api.Types (Token, TabbycatInstance)
import CommonCmdArgs (tabbycatInstance, tabbycatToken)

data CmdArgs = CmdArgs
  { tabbycatToken :: Token
  , tabbycatInstance :: TabbycatInstance
  , baseDir :: FilePath
  , emailTableFile :: FilePath
  , hiddenQuestions :: [Text]
  , randomizeOrder :: Bool
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
  <$> tabbycatToken
  <*> tabbycatInstance
  <*> strOption
        (long "basedir" <>
         metavar "DIR" <>
         value "." <>
         help "Directory where the generated HTML files will be stored.")
  <*> strOption
        (long "email-table" <>
         metavar "FILE" <>
         value "emails.csv" <>
         help "Filepath for the email table.")
  <*> many (strOption
        (long "omit" <>
         metavar "QUESTION_TEXT" <>
         help "Omit a question from the generated HTML. Takes the question text as an argument. Can be given multiple times."))
  <*> flag True False
        (long "no-randomize" <>
         help "Don't randomize the order of feedback sheets and show which round each sheet is from.")
