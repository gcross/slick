{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Transition where

import Control.Lens (Lens',set,use)

import Slick.Animation
import Slick.Presentation

class Interpolatable t s where
    interpolateUnitInterval :: s → s → t → s

instance (t ~ s, Num t) ⇒ Interpolatable t s where
    interpolateUnitInterval start end t = start*(1-t) + end*t

easeAnimation :: (Timelike t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → s' → Animation t s
easeAnimation transition lens duration start end = clampAnimation $
    cachelessAnimation
        duration
        (set lens . interpolateUnitInterval start end . transition . (/duration))

type Transition t = t → t

easeFromTo :: ∀ t s s'. (Timelike t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → s' → Presentation t s ()
easeFromTo transition lens duration start end = appendAnimation animation
  where
    animation :: Animation t s
    animation = easeAnimation transition lens duration start end

easeTo :: (Timelike t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → Presentation t s ()
easeTo transition lens duration end = do
    start ← use lens
    easeFromTo transition lens duration start end

easeBy :: (Timelike t, Num s', Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → Presentation t s ()
easeBy transition lens duration difference = do
    start ← use lens
    let end = start + difference
    easeFromTo transition lens duration start end

easeByFactor :: (Timelike t, Num s', Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → Presentation t s ()
easeByFactor transition lens duration factor = do
    start ← use lens
    let end = start * factor
    easeFromTo transition lens duration start end

clampTransition :: Timelike t ⇒ Transition t → Transition t
clampTransition transition t
  | t < 0 = 0
  | t > 1 = 1
  | otherwise = transition t

linear_transition :: Timelike t ⇒ Transition t
linear_transition = clampTransition id

linearFromTo :: (Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
linearFromTo = easeFromTo linear_transition

linearTo :: (Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
linearTo = easeTo linear_transition

linearBy :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
linearBy = easeBy linear_transition

linearByFactor :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
linearByFactor = easeByFactor linear_transition

smooth_transition :: Timelike t ⇒ Transition t
smooth_transition = clampTransition $ \t → sin(pi*t/2)**2

smoothFromTo :: (Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
smoothFromTo = easeFromTo smooth_transition

smoothTo :: (Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
smoothTo = easeTo smooth_transition

smoothBy :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
smoothBy = easeBy smooth_transition

smoothByFactor :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
smoothByFactor = easeByFactor smooth_transition

decelerate_transition :: Timelike t ⇒ Transition t
decelerate_transition = clampTransition $ \t → sin(pi*t/2)

decelerateFromTo :: (Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
decelerateFromTo = easeFromTo decelerate_transition

decelerateTo :: (Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
decelerateTo = easeTo decelerate_transition

decelerateBy :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
decelerateBy = easeBy decelerate_transition

decelerateByFactor :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
decelerateByFactor = easeByFactor decelerate_transition

accelerate_transition :: Timelike t ⇒ Transition t
accelerate_transition = clampTransition $ \t → 1-cos(pi*t/2)

accelerateFromTo :: (Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
accelerateFromTo = easeFromTo accelerate_transition

accelerateTo :: (Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
accelerateTo = easeTo accelerate_transition

accelerateBy :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
accelerateBy = easeBy accelerate_transition

accelerateByFactor :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
accelerateByFactor = easeByFactor accelerate_transition
