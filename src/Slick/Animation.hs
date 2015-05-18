{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation where

import Control.Exception (assert)

import Data.Composition ((.**))
import Data.List (mapAccumL)

data Animation α β = ∀ ɣ. Animation
    { animationDuration :: α
    , animationCache :: ɣ
    , animationFunction :: α → (β → ɣ → (β, ɣ))
    }

null_animation :: Num α ⇒ Animation α β
null_animation = Animation 0 () (\_ x y → (x,y))

cachelessAnimation :: α → (α → β → β) → Animation α β
cachelessAnimation duration function = Animation duration () (\t x () → (function t x, ()))

statelessAnimation :: α → (α → α) → Animation α α
statelessAnimation duration function = Animation duration () (\t _ () → (function t, ()))

durationOf :: Animation α β → α
durationOf animation = case animation of Animation{..} → animationDuration

runAnimation :: α → β → Animation α β → (β, Animation α β)
runAnimation t state Animation{..} =
    (new_state, Animation animationDuration new_cache animationFunction)
  where
    (new_state, new_cache) = animationFunction t state animationCache

runAnimationState :: α → β → Animation α β → β
runAnimationState = fst .** runAnimation

data Side = LeftSide | RightSide

serial [] = null_animation
serial [animation] = animation
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
                let (new_state,new_x) = runAnimation time state x
                in (new_state,(new_x,y,LeftSide))
          | otherwise =
                case last_side of
                    LeftSide →
                        let (new_state,new_x) = runAnimation duration_of_x state x
                            (final_state,new_y) = runAnimation (time - durationOf x) new_state y
                        in (final_state,(new_x,new_y,RightSide))
                    RightSide →
                        let (new_state,new_y) = runAnimation (time - durationOf x) state y
                        in (new_state,(x,new_y,RightSide))

parallel :: (Num α, Ord α) ⇒ [Animation α β] → Animation α β
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
                in runAnimation clamped_time state animation
            )
            state
            list

