{-# LANGUAGE TemplateHaskell #-}
module PrivateURLExport.Static (stylesheet) where

import Data.ByteString (ByteString)
import Data.FileEmbed

stylesheet :: ByteString
stylesheet = $(embedFile =<< makeRelativeToProject "static/PrivateURLExport/style.css")
