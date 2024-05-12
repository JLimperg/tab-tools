module FeedbackCSVExport.CmdArgs (CmdArgs(..), cmdArgs) where

import Options.Applicative

import Api.Types
import CommonCmdArgs

data CmdArgs = CmdArgs
  { token :: Token
  , tabbycatInstance :: TabbycatInstance
  , outputFile :: FilePath
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
  <$> tabbycatToken
  <*> tabbycatInstance
  <*> (strOption
       (short 'o' <>
        long "output" <>
        metavar "FILE" <>
        value "output/feedback.csv" <>
        help "Path of the output CSV file."))
