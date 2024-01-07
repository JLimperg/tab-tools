module OpenTabInput
( readOpenTab )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.HTTP.Req (Url, Scheme(Https), https, (/:))
import System.Exit (exitFailure)
import Types (Participant(..))

mkPrivateUrl :: Text -> Text -> Url 'Https
mkPrivateUrl remoteId key =
  https "opentab.limperg.de" /: "url" /: (remoteId <> "_" <> key)

parseParticipant :: Text -> Either String Participant
parseParticipant line = do
  let fields = Text.splitOn "|" line
  case fields of
    [firstName,lastName,remoteId,key] ->
      Right $ Participant
        { name = firstName <> " " <> lastName
        , urlKey = key
        , privateUrl = mkPrivateUrl remoteId key
        }
    _ ->
      Left $ "Not enough fields on this line:\n" ++ Text.unpack line


readOpenTab :: FilePath -> IO [Participant]
readOpenTab fp = do
  input <- Text.readFile fp
  let result = traverse parseParticipant $ Text.lines input
  case result of
    Left err -> do
      putStrLn err
      exitFailure
    Right res ->
      pure res
