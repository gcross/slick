{-# LANGUAGE UnicodeSyntax #-}

module Slick.Renderer.Common where

fixSize :: (RealFrac α, Integral β, Integral ɣ) ⇒ α → β → β → (ɣ, ɣ)
fixSize correct_aspect_ratio width height = (fixed_width, fixed_height)
  where
    width_f = fromIntegral width
    height_f = fromIntegral height
    current_aspect_ratio = width_f/height_f
    fixed_width = round $
        if current_aspect_ratio > correct_aspect_ratio
        then width_f / current_aspect_ratio * correct_aspect_ratio
        else width_f
    fixed_height = round $
        if current_aspect_ratio < correct_aspect_ratio
        then height_f * current_aspect_ratio / correct_aspect_ratio
        else height_f

