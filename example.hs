{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Codec.Picture(PixelRGBA8(..))

import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Slick.Renderer.Rasterific
import Slick.Renderer.Rasterific.SDL

graphic = Graphic 400 200 white $
    withTexture (uniformTexture drawColor) $ do
        fill $ circle (V2 0 0) 30
        stroke 4 JoinRound (CapRound, CapRound) $
               circle (V2 400 200) 40
        withTexture (uniformTexture recColor) .
               fill $ rectangle (V2 100 100) 200 100
  where
    white = PixelRGBA8 255 255 255 255
    drawColor = PixelRGBA8 0 0x86 0xc1 255
    recColor = PixelRGBA8 0xFF 0x53 0x73 255

main = mainViewing graphic

