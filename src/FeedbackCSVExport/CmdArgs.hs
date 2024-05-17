module FeedbackCSVExport.CmdArgs (CmdArgs(..), cmdArgs) where

import Options.Applicative

import Api.Types
import CommonCmdArgs

data CmdArgs = CmdArgs
  { token :: Token
  , tabbycatInstance :: TabbycatInstance
  , outputFile :: FilePath
  , oldFeedbackFile :: Maybe FilePath
  , omitIgnored :: Bool
  , omitUnconfirmed :: Bool
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
  <*> (optional $ strOption
        (long "old-feedback" <>
         metavar "FILE" <>
         help "Path of a previously generated CSV file. Feedbacks already present in this file will not be exported."))
  <*> flag True False
        (long "omit-ignored" <>
         help "Don't export ignored feedbacks.")
  <*> flag True False
        (long "omit-unconfirmed" <>
         help "Don't export unconfirmed feedbacks.")
