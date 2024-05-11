module PrivateURLExport.CmdArgs
( Command(..)
, parseCmdArgs
) where

import Options.Applicative

import Api.Types (Token, TabbycatInstance)
import CommonCmdArgs (tabbycatInstance, tabbycatToken)

data Command
  = Tabbycat TabbycatInstance Token
  | OpenTab FilePath

tabbycatCommand :: Mod CommandFields Command
tabbycatCommand =
  command "tabbycat"
    (info tabbycatOptions (progDesc "Export Tabbycat private links"))

tabbycatOptions :: Parser Command
tabbycatOptions = Tabbycat <$> tabbycatInstance <*> tabbycatToken

openTabCommand :: Mod CommandFields Command
openTabCommand =
  command "opentab"
    (info openTabOptions (progDesc "Export open_tab private links"))

openTabOptions :: Parser Command
openTabOptions = OpenTab
  <$> strOption
        (long "file" <>
         metavar "FILE" <>
         help "Input file. Each line must be of the form <first_name>|<last_name>|<remote_id>|<url_key>")

cmdArgs :: Parser Command
cmdArgs = hsubparser (tabbycatCommand <> openTabCommand)

cmdInfo :: ParserInfo Command
cmdInfo = info (helper <*> cmdArgs) mempty

parseCmdArgs :: IO Command
parseCmdArgs = execParser cmdInfo
