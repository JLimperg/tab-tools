module Main (main) where

import Options.Applicative

import CSVImport.Main qualified as CSVImport
import CSVImport.CmdArgs qualified as CSVImport
import FeedbackCSVExport.Main qualified as FeedbackCSVExport
import FeedbackCSVExport.CmdArgs qualified as FeedbackCSVExport
import FeedbackHTMLExport.Main qualified as FeedbackHTMLExport
import FeedbackHTMLExport.CmdArgs qualified as FeedbackHTMLExport
import PrivateURLExport.Main qualified as PrivateURLExport
import PrivateURLExport.CmdArgs qualified as PrivateURLExport

data Command
  = CSVImport CSVImport.CmdArgs
  | FeedbackCSVExport FeedbackCSVExport.CmdArgs
  | FeedbackHTMLExport FeedbackHTMLExport.CmdArgs
  | PrivateURLExport PrivateURLExport.Command

csvImportCommand :: Mod CommandFields Command
csvImportCommand = command "import-participants" $
  info (CSVImport <$> CSVImport.cmdArgs) $
    progDesc "Import speakers and adjudicators from CSV files into Tabbycat"

feedbackCsvCommand :: Mod CommandFields Command
feedbackCsvCommand = command "export-feedback-csv" $
  info (FeedbackCSVExport <$> FeedbackCSVExport.cmdArgs) $
    progDesc "Export CSV file with judge feedback from Tabbycat"

feedbackHtmlCommand :: Mod CommandFields Command
feedbackHtmlCommand = command "export-feedback-html" $
  info (FeedbackHTMLExport <$> FeedbackHTMLExport.cmdArgs) $
    progDesc "Export HTML files with anonymised judge feedback from Tabbycat"

privateURLExportCommand :: Mod CommandFields Command
privateURLExportCommand = command "export-private-urls" $
  info (PrivateURLExport <$> PrivateURLExport.cmdArgs) $
    progDesc "Export private URL QR codes from Tabbycat or Opentab"

cmdArgs :: Parser Command
cmdArgs = hsubparser $
  csvImportCommand <> feedbackCsvCommand <> feedbackHtmlCommand <>
  privateURLExportCommand

cmdInfo :: ParserInfo Command
cmdInfo = info (helper <*> cmdArgs) mempty

main :: IO ()
main = do
  cmd <- execParser cmdInfo
  case cmd of
    CSVImport args -> CSVImport.main args
    FeedbackCSVExport args -> FeedbackCSVExport.main args
    FeedbackHTMLExport args -> FeedbackHTMLExport.main args
    PrivateURLExport args -> PrivateURLExport.main args
