module CommonCmdArgs
( tabbycatInstance
, tabbycatToken
) where

import Options.Applicative
import Api.Types (Token(..), TabbycatInstance(..))

tabbycatInstance :: Parser TabbycatInstance
tabbycatInstance = TabbycatInstance
  <$> strOption
        (long "host" <>
         help "Tabbycat host, e.g. tabbycat.limperg.de")
  <*> strOption
        (long "tournament" <>
         help "Tabbycat tournament URL slug, e.g. wudc2023")

tabbycatToken :: Parser Token
tabbycatToken = Token <$> strOption
  (long "token" <>
   metavar "TOKEN" <>
   help "Tabbycat API token.")
