{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Interpolation where

import Control.Lens (Lens',(%=),set,use)

import Slick.Animation
import Slick.AnimationM

class Interpolatable t s where
    interpolateUnitInterval :: s → s → t → s

instance (t ~ s, Num t) ⇒ Interpolatable t s where
    interpolateUnitInterval t start end = start*(1-t) + end*t

easeAnimation :: (Fractional t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → s' → Animation t s
easeAnimation transition lens duration start end =
    cachelessAnimation
        duration
        (set lens . interpolateUnitInterval start end . transition . (/duration))

type Transition t = t → t


easeFromTo :: ∀ t s s'. (Timelike t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → s' → AnimationM t s ()
easeFromTo transition lens duration start end = do
    let animation :: Animation t s
        animation = easeAnimation transition lens duration start end
        duration = durationOf animation
    appendAnimation animation
    ams_state %= set lens end
    updateTimeAndDurationWithDuration duration
    appendAnimation animation

easeTo :: (Timelike t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → AnimationM t s ()
easeTo transition lens duration end = do
    start ← use (ams_state . lens)
    easeFromTo transition lens duration start end

easeBy :: (Timelike t, Num s', Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → AnimationM t s ()
easeBy transition lens duration difference = do
    start ← use (ams_state . lens)
    let end = start + difference
    easeFromTo transition lens duration start end

linear_transition = id
linearFromTo = easeFromTo linear_transition
linearTo = easeTo linear_transition
linearBy = easeBy linear_transition

smooth_transition t = sin(pi*t/2)**2
smoothFromTo = easeFromTo smooth_transition
smoothTo = easeTo smooth_transition
smoothBy = easeBy smooth_transition

decelerate_transition t = sin(pi*t/2)
decelerateFromTo = easeFromTo decelerate_transition
decelerateTo = easeTo decelerate_transition
decelerateBy = easeBy decelerate_transition

accelerate_transition t = 1-sin(pi*t/2)
accelerateFromTo = easeFromTo accelerate_transition
accelerateTo = easeTo accelerate_transition
accelerateBy = easeBy accelerate_transition
