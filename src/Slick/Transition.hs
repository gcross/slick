{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Transition where

import Control.Lens (Lens',(%=),set,use)

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

linear_transition :: Transition t
linear_transition = id

linearFromTo :: (Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
linearFromTo = easeFromTo linear_transition

linearTo :: (Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
linearTo = easeTo linear_transition

linearBy :: (Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
linearBy = easeBy linear_transition

smooth_transition :: (Floating t, Timelike t) ⇒ Transition t
smooth_transition t = sin(pi*t/2)**2

smoothFromTo :: (Floating t, Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
smoothFromTo = easeFromTo linear_transition

smoothTo :: (Floating t, Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
smoothTo = easeTo linear_transition

smoothBy :: (Floating t, Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
smoothBy = easeBy linear_transition

decelerate_transition :: (Floating t, Timelike t) ⇒ Transition t
decelerate_transition t = sin(pi*t/2)

decelerateFromTo :: (Floating t, Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
decelerateFromTo = easeFromTo decelerate_transition

decelerateTo :: (Floating t, Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
decelerateTo = easeTo decelerate_transition

decelerateBy :: (Floating t, Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
decelerateBy = easeBy decelerate_transition

accelerate_transition :: (Floating t, Timelike t) ⇒ Transition t
accelerate_transition t = 1-sin(pi*t/2)

accelerateFromTo :: (Floating t, Timelike t, Interpolatable t s') ⇒  Lens' s s' → t → s' → s' → Presentation t s ()
accelerateFromTo = easeFromTo accelerate_transition

accelerateTo :: (Floating t, Timelike t, Interpolatable t s') ⇒ Lens' s s' → t → s' → Presentation t s ()
accelerateTo = easeTo accelerate_transition

accelerateBy :: (Floating t, Timelike t, Num s', Interpolatable t s') ⇒  Lens' s s' → t → s' → Presentation t s ()
accelerateBy = easeBy accelerate_transition

