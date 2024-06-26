module PrivateURLExport.Main (main) where

import Data.List (sortOn)
import Network.HTTP.Req (Url, Scheme(Https), (/:), https)

import Api
import Api.Types
import PrivateURLExport.CmdArgs
import PrivateURLExport.OpenTabInput
import PrivateURLExport.Render
import PrivateURLExport.Types

mkPrivateUrl :: TabbycatInstance -> GetParticipantsResponse -> Url 'Https
mkPrivateUrl inst resp =
  https inst.host /: inst.tournament /: "privateurls" /: resp.url_key

participantReqToParticipant ::
  TabbycatInstance -> GetParticipantsResponse -> Participant
participantReqToParticipant inst resp = Participant
  { name = resp.name
  , urlKey = resp.url_key
  , privateUrl = mkPrivateUrl inst resp
  }

renderParticipants :: [Participant] -> IO ()
renderParticipants participants =
  render "output" $ sortOn (\x -> x.name) participants

getTabbycatParticipants :: ApiM [Participant]
getTabbycatParticipants = do
  speakers <- getSpeakers
  adjudicators <- getAdjudicators
  inst <- getTabbycatInstance
  pure $ map (participantReqToParticipant inst) $ speakers ++ adjudicators

main :: Command -> IO ()
main cmd = do
  participants <-
    case cmd of
      Tabbycat inst token -> runApiM token inst getTabbycatParticipants
      OpenTab fp -> readOpenTab fp
  renderParticipants participants
