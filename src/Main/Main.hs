module Main (main) where

import Options.Applicative

import CSVImport.Main qualified as CSVImport
import CSVImport.CmdArgs qualified as CSVImport
import FeedbackHTMLExport.Main qualified as FeedbackHTMLExport
import FeedbackHTMLExport.CmdArgs qualified as FeedbackHTMLExport
import PrivateURLExport.Main qualified as PrivateURLExport
import PrivateURLExport.CmdArgs qualified as PrivateURLExport

data Command
  = CSVImport CSVImport.CmdArgs
  | FeedbackHTMLExport FeedbackHTMLExport.CmdArgs
  | PrivateURLExport PrivateURLExport.Command

csvImportCommand :: Mod CommandFields Command
csvImportCommand = command "import-participants" $
  info (CSVImport <$> CSVImport.cmdArgs) $
    progDesc "Import speakers and adjudicators from CSV files into Tabbycat"

feedbackCommand :: Mod CommandFields Command
feedbackCommand = command "export-feedback-html" $
  info (FeedbackHTMLExport <$> FeedbackHTMLExport.cmdArgs) $
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
    FeedbackHTMLExport args -> FeedbackHTMLExport.main args
    PrivateURLExport args -> PrivateURLExport.main args
