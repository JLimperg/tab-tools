{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Render (render) where

import           Prelude hiding (head, div)

import qualified Data.QRCode as QR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromJust)
import           Data.String (fromString)
import qualified Data.Text as Text
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import qualified Diagrams.QRCode as QR
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), (<.>))
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes as Html
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Network.HTTP.Req (Url, Scheme(Https), (/:), https, renderUrl)

import           Types (Participant(..), TabbycatInstance(..))
import           Static (stylesheet)

qrFilename :: Participant -> FilePath
qrFilename Participant { urlKey } =
  Text.unpack urlKey <.> "svg"

render :: FilePath -> [Participant] -> IO ()
render baseDir participants = do
  createDirectoryIfMissing True baseDir
  writeParticipants baseDir participants
  writeQRCodes baseDir participants
  writeStylesheet baseDir

writeStylesheet :: FilePath -> IO ()
writeStylesheet baseDir = BS.writeFile (baseDir ++ "/style.css") stylesheet

writeParticipants :: FilePath -> [Participant] -> IO ()
writeParticipants baseDir participants
  = let filename = baseDir </> "private-urls.html" in
    BSL.writeFile filename $ renderHtml $
      renderParticipants participants

writeQRCodes :: FilePath -> [Participant] -> IO ()
writeQRCodes baseDir = mapM_ (writeQRCode baseDir)

writeQRCode :: FilePath -> Participant -> IO ()
writeQRCode baseDir p = do
  dia <- renderQRCode p
  D.renderSVG (baseDir </> qrFilename p) (D.dims2D 250 250) dia

renderQRCode :: Participant -> IO (D.QDiagram D.SVG D.V2 Double D.Any)
renderQRCode Participant { privateUrl } = do
  let content = Text.unpack $ renderUrl privateUrl
  qrcode <- QR.encodeString content Nothing QR.QR_ECLEVEL_Q QR.QR_MODE_EIGHT True
  pure $ D.scale 6 $ QR.stroke $ QR.pathMatrix $ QR.toMatrix qrcode

renderParticipants :: [Participant] -> Html
renderParticipants participants = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    link ! rel "stylesheet" ! href "style.css"
  body $ table $ do
    mapM_ renderParticipant participants

renderParticipant :: Participant -> Html
renderParticipant p@Participant { name, privateUrl } =
  tr $ do
    td ! class_ "col1" $ do
      div ! class_ "name" $ text name
      div ! class_ "url" $ text $ renderUrl privateUrl
    td ! class_ "col2" $
      img ! class_ "qrcode" ! src (fromString $ qrFilename p)
