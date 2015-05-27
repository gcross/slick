{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation where

import Control.Exception (assert)
import Control.Lens (Lens',(^.),set)

import Data.Composition ((.**))
import Data.List (mapAccumL)

data Animation t s = ∀ ɣ. Animation
    { animationDuration :: t
    , animationCache :: ɣ
    , animationFunction :: t → (s → ɣ → (s, ɣ))
    }

promoteAnimation :: Lens' s s' → Animation α s' → Animation α s
promoteAnimation alens Animation{..} =
    Animation animationDuration animationCache newAnimationFunction
  where
    newAnimationFunction t s c = (s_new,c_new)
      where
        (s'_new,c_new) = animationFunction t (s ^. alens) c
        s_new = set alens s'_new s

clampAnimation :: (Num t, Ord t) ⇒ Animation t s → Animation t s
clampAnimation (Animation duration cache f) =
  Animation duration cache $ \t s c →
    if | t < 0 → f 0 s c
       | t > duration → f duration s c
       | otherwise → f t s c

null_animation :: Num t ⇒ Animation t s
null_animation = Animation 0 () (\_ x y → (x,y))

cachelessAnimation :: (Num t, Ord t) ⇒ t →  (t → s → s) → Animation t s
cachelessAnimation duration function = clampAnimation $ Animation duration () (\t x () → (function t x, ()))

statelessAnimation :: (Num t, Ord t) ⇒ t → (t → t) → Animation t t
statelessAnimation duration function = clampAnimation $ Animation duration () (\t _ () → (function t, ()))

zeroTimeAnimation :: (Num t, Ord t) ⇒ s → Animation t s
zeroTimeAnimation new_state = cachelessAnimation 0 (const . const $ new_state)

durationOf :: Animation t s → t
durationOf animation = case animation of Animation{..} → animationDuration

runAnimation :: Animation t s → t → s → (s, Animation t s)
runAnimation Animation{..} t state =
    (new_state, Animation animationDuration new_cache animationFunction)
  where
    (new_state, new_cache) = animationFunction t state animationCache

execAnimation :: Animation t s → t → s → s
execAnimation = fst .** runAnimation

data Side = LeftSide | RightSide

serial [] = null_animation
serial [animation] = clampAnimation animation
serial animations = serial $ merge animations
  where
    merge [] = []
    merge [x] = [x]
    merge (x:y:rest) = Animation{..}:merge rest
      where
        duration_of_x = durationOf x

        animationDuration = duration_of_x + durationOf y
        animationCache = (x,y,LeftSide)
        animationFunction time state (x,y,last_side)
          | time < duration_of_x =
                let (new_state,new_x) = runAnimation x time state
                in (new_state,(new_x,y,LeftSide))
          | otherwise =
                case last_side of
                    LeftSide →
                        let (new_state,new_x) = runAnimation x duration_of_x state
                            (final_state,new_y) = runAnimation y (time - durationOf x) new_state
                        in (final_state,(new_x,new_y,RightSide))
                    RightSide →
                        let (new_state,new_y) = runAnimation y (time - durationOf x) state
                        in (new_state,(x,new_y,RightSide))

parallel :: (Num t, Ord t) ⇒ [Animation t s] → Animation t s
parallel [] = null_animation
parallel animations = Animation animationDuration animationCache animationFunction -- (Animation{..})
  where
    animationDuration = maximum . map durationOf $ animations

    animationCache = animations

    animationFunction time state list
      | time < 0 =
          animationFunction 0 state list
      | otherwise =
          mapAccumL
            (\state animation@Animation{..} →
                let clamped_time = if time > animationDuration then animationDuration else time
                in runAnimation animation clamped_time state
            )
            state
            list

