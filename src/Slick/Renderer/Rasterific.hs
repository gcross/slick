{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Renderer.Rasterific where

import Codec.Picture (Image(imageData),PixelRGBA8)

import Data.Vector.Storable (Vector)
import Data.Word (Word8)

import Graphics.Rasterific (Drawing, renderDrawing, withTransformation)
import Graphics.Rasterific.Transformations (scale)

data Graphic = Graphic
    {   graphicWidth :: Float
    ,   graphicHeight :: Float
    ,   graphicBackground :: PixelRGBA8
    ,   graphicDrawing :: Drawing PixelRGBA8 ()
    }

graphicAspectRatio :: Graphic → Float
graphicAspectRatio Graphic{..} = graphicWidth / graphicHeight

renderGraphic :: Graphic → Int → Int → Vector Word8
renderGraphic Graphic{..} screen_width screen_height =
    imageData
    .
    renderDrawing screen_width screen_height graphicBackground
    .
    withTransformation (
        scale (fromIntegral screen_width/graphicWidth)
              (fromIntegral screen_height/graphicHeight)
    )
    $
    graphicDrawing
