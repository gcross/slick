{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Transition where

import Control.Lens (Lens',set,use)

import Slick.Animation
import Slick.Presentation

type Progress = Double

type Easing = Progress → Progress

class Interpolatable s where
    interpolateUnitInterval :: s → s → Progress → s

instance Fractional s ⇒ Interpolatable s where
    interpolateUnitInterval start end t = start*(1-t') + end*t'
      where
        t' = realToFrac t

easeAnimation :: Interpolatable s' ⇒ Easing → Lens' s s' → Duration → s' → s' → Animation s
easeAnimation easing lens duration start end = clampAnimation $
    cachelessAnimation
        duration
        (set lens . interpolateUnitInterval start end . easing . (/duration))

easeFromTo :: ∀ t s s'. Interpolatable s' ⇒ Easing → Lens' s s' → Duration → s' → s' → PresentationM s ()
easeFromTo easing lens duration start end = appendAnimation animation
  where
    animation :: Animation s
    animation = easeAnimation easing lens duration start end

easeTo :: Interpolatable s' ⇒ Easing → Lens' s s' → Duration → s' → PresentationM s ()
easeTo easing lens duration end = do
    start ← use lens
    easeFromTo easing lens duration start end

easeBy :: (Num s', Interpolatable s') ⇒ Easing → Lens' s s' → Duration → s' → PresentationM s ()
easeBy easing lens duration difference = do
    start ← use lens
    let end = start + difference
    easeFromTo easing lens duration start end

easeByFactor :: (Num s', Interpolatable s') ⇒ Easing → Lens' s s' → Duration → s' → PresentationM s ()
easeByFactor easing lens duration factor = do
    start ← use lens
    let end = start * factor
    easeFromTo easing lens duration start end

clampEasing :: Easing → Easing
clampEasing easing t
  | t < 0 = 0
  | t > 1 = 1
  | otherwise = easing t

linear_easing :: Easing
linear_easing = clampEasing id

linearFromTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → s' → PresentationM s ()
linearFromTo = easeFromTo linear_easing

linearTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → PresentationM s ()
linearTo = easeTo linear_easing

linearBy :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
linearBy = easeBy linear_easing

linearByFactor :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
linearByFactor = easeByFactor linear_easing

smooth_easing :: Easing
smooth_easing = clampEasing $ \t → sin(pi*t/2)**2

smoothFromTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → s' → PresentationM s ()
smoothFromTo = easeFromTo smooth_easing

smoothTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → PresentationM s ()
smoothTo = easeTo smooth_easing

smoothBy :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
smoothBy = easeBy smooth_easing

smoothByFactor :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
smoothByFactor = easeByFactor smooth_easing

deceleration_easing :: Easing
deceleration_easing = clampEasing $ \t → sin(pi*t/2)

decelerateFromTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → s' → PresentationM s ()
decelerateFromTo = easeFromTo deceleration_easing

decelerateTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → PresentationM s ()
decelerateTo = easeTo deceleration_easing

decelerateBy :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
decelerateBy = easeBy deceleration_easing

decelerateByFactor :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
decelerateByFactor = easeByFactor deceleration_easing

acceleration_easing :: Easing
acceleration_easing = clampEasing $ \t → 1-cos(pi*t/2)

accelerateFromTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → s' → PresentationM s ()
accelerateFromTo = easeFromTo acceleration_easing

accelerateTo :: Interpolatable s' ⇒ Lens' s s' → Duration → s' → PresentationM s ()
accelerateTo = easeTo acceleration_easing

accelerateBy :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
accelerateBy = easeBy acceleration_easing

accelerateByFactor :: (Num s', Interpolatable s') ⇒ Lens' s s' → Duration → s' → PresentationM s ()
accelerateByFactor = easeByFactor acceleration_easing
