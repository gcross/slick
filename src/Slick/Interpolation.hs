{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Interpolation where

import Control.Lens (Lens',set)

import Slick.Animation

class Interpolatable t s where
    interpolateUnitInterval :: s → s → t → s

instance (t ~ s, Num t) ⇒ Interpolatable t s where
    interpolateUnitInterval t start end = start*(1-t) + end*t

easeAnimation :: (Fractional t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → s' → Animation t s
easeAnimation transition lens duration start end =
    cachelessAnimation
        duration
        (set lens . interpolateUnitInterval start end . transition . (/duration))
