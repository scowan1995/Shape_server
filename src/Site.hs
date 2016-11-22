{-# LANGUAGE OverloadedStrings #-}
module Site where

import qualified Web.Scotty as S
import qualified Shapes as Shape
import qualified Data.Text.Lazy as L
import qualified Data.Text.Encoding as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Monoid (mconcat)

runSite =
    S.scotty 3000 $ do
  S.get "/" $ S.file "./src/enter.html"
  S.get "/drawing" $ do
    draw <- S.param "drawing"
    S.html $ L.pack $ Shape.convert (read draw)
