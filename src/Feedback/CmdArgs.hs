module CmdArgs (CmdArgs(..), parseCmdArgs) where

import           Options.Applicative
import           Data.Text (Text)

import Types (Token(..))

data CmdArgs = CmdArgs
  { token :: Token
  , baseURL :: Text
  , baseDir :: FilePath
  , emailTableFile :: FilePath
  , hiddenQuestions :: [Text]
  , randomizeOrder :: Bool
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
  <$> (Token <$> strOption
        (long "token" <>
         metavar "TOKEN" <>
         help "Tabbycat API token."))
  <*> strOption
        (long "url" <>
         metavar "URL" <>
         help "Base Tabbycat API URL for the tournament. Usually looks like https://<dom>.herokuapp.com/api/v1/tournaments/<tournament-slug>")
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

cmdInfo :: ParserInfo CmdArgs
cmdInfo = info cmdArgs mempty

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser cmdInfo
