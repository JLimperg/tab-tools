module CSVImport.CmdArgs (CmdArgs(..), cmdArgs) where

import Options.Applicative

import CommonCmdArgs (tabbycatInstance, tabbycatToken)
import Api.Types (Token, TabbycatInstance)

data CmdArgs = CmdArgs
  { tabbycatInstance :: TabbycatInstance
  , tabbycatToken :: Token
  , speakersFile :: FilePath
  , adjudicatorsFile :: FilePath
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
  <$> tabbycatInstance
  <*> tabbycatToken
  <*> strOption
        (long "speakers" <>
         metavar "FILE" <>
         help "CSV file containing speaker data.")
  <*> strOption
        (long "adjudicators" <>
         metavar "FILE" <>
         help "CSV file containing adjudicator data.")
