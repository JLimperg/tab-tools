module Main (main) where

import Options.Applicative

import CSVImport.Main qualified as CSVImport
import CSVImport.CmdArgs qualified as CSVImport
import Feedback.Main qualified as Feedback
import Feedback.CmdArgs qualified as Feedback
import PrivateURLExport.Main qualified as PrivateURLExport
import PrivateURLExport.CmdArgs qualified as PrivateURLExport

data Command
  = CSVImport CSVImport.CmdArgs
  | Feedback Feedback.CmdArgs
  | PrivateURLExport PrivateURLExport.Command

csvImportCommand :: Mod CommandFields Command
csvImportCommand = command "import-participants" $
  info (CSVImport <$> CSVImport.cmdArgs) $
    progDesc "Import speakers and adjudicators from CSV files into Tabbycat"

feedbackCommand :: Mod CommandFields Command
feedbackCommand = command "export-feedback-html" $
  info (Feedback <$> Feedback.cmdArgs) $
    progDesc "Export HTML files with anonymised judge feedback from Tabbycat"

privateURLExportCommand :: Mod CommandFields Command
privateURLExportCommand = command "export-private-urls" $
  info (PrivateURLExport <$> PrivateURLExport.cmdArgs) $
    progDesc "Export private URL QR codes from Tabbycat or Opentab"

cmdArgs :: Parser Command
cmdArgs = hsubparser $
  csvImportCommand <> feedbackCommand <> privateURLExportCommand

cmdInfo :: ParserInfo Command
cmdInfo = info (helper <*> cmdArgs) mempty

main :: IO ()
main = do
  cmd <- execParser cmdInfo
  case cmd of
    CSVImport args -> CSVImport.main args
    Feedback args -> Feedback.main args
    PrivateURLExport args -> PrivateURLExport.main args
