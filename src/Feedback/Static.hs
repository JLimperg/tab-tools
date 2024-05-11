{-# LANGUAGE TemplateHaskell #-}
module Feedback.Static (stylesheet) where

import Data.ByteString (ByteString)
import Data.FileEmbed

stylesheet :: ByteString
stylesheet = $(embedFile =<< makeRelativeToProject "static/Feedback/style.css")
