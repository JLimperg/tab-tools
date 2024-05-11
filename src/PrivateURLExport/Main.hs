module Main (main) where

import           Data.List (sortOn)
import           Network.HTTP.Req (Url, Scheme(Https), (/:), https)

import           PrivateURLExport.Api
import           PrivateURLExport.CmdArgs
import           PrivateURLExport.OpenTabInput
import           PrivateURLExport.Render
import           PrivateURLExport.Types

mkPrivateUrl :: TabbycatInstance -> ParticipantReq -> Url 'Https
mkPrivateUrl TabbycatInstance { host, tournament } ParticipantReq { url_key } =
  https host /: tournament /: "privateurls" /: url_key

participantReqToParticipant :: TabbycatInstance -> ParticipantReq -> Participant
participantReqToParticipant tabbycat p@ParticipantReq { name, url_key } =
  Participant { name, urlKey = url_key, privateUrl = mkPrivateUrl tabbycat p }

renderParticipants :: [Participant] -> IO ()
renderParticipants participants =
  render "output" $ sortOn (\x -> x.name) participants

getTabbycatParticipants :: TabbycatInstance -> Token -> IO [Participant]
getTabbycatParticipants inst token = do
  speakers <- getSpeakers inst token
  adjudicators <- getAdjudicators inst token
  pure $ map (participantReqToParticipant inst) $ speakers ++ adjudicators

main :: IO ()
main = do
  cmd <- parseCmdArgs
  participants <-
    case cmd of
      Tabbycat inst token -> getTabbycatParticipants inst token
      OpenTab fp -> readOpenTab fp
  renderParticipants participants
