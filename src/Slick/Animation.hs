{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation where

import Control.Exception (assert)
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

runAnimation :: Animation α β → α → β → (β, Animation α β)
runAnimation Animation{..} t state =
    (new_state, Animation animationDuration new_cache animationFunction)
  where
    (new_state, new_cache) = animationFunction t state animationCache

data AnimationZipper α β = AnimationZipper
    { zipperLeft :: [Animation α β]
    , zipperRight :: [Animation α β]
    , zipperCurrent :: Animation α β
    , zipperLeftTime :: α
    }

moveLeft :: Num α ⇒ β → AnimationZipper α β → (β, AnimationZipper α β)
moveLeft state zipper@AnimationZipper{zipperLeft=[]} = (state, zipper)
moveLeft state (AnimationZipper (left:rest) right current left_time) = (new_state, new_zipper)
  where
    new_state = case current of
                    Animation{..} →
                        fst $ animationFunction left_time
                                                state
                                                animationCache
    new_zipper =
        AnimationZipper
        { zipperLeft = rest
        , zipperRight = current:right
        , zipperCurrent = left
        , zipperLeftTime = left_time - durationOf current
        }

moveRight :: Num α ⇒ β → AnimationZipper α β → (β, AnimationZipper α β)
moveRight state zipper@AnimationZipper{zipperRight=[]} = (state, zipper)
moveRight state (AnimationZipper left (right:rest) current left_time) = (new_state, new_zipper)
  where
    new_state = case current of
                    Animation{..} →
                        fst $ animationFunction (left_time + animationDuration)
                                                state
                                                animationCache
    new_zipper =
        AnimationZipper
        { zipperLeft = current:left
        , zipperRight = rest
        , zipperCurrent = right
        , zipperLeftTime = left_time + durationOf current
        }

serial :: (Num α, Ord α) ⇒ [Animation α β] → Animation α β
serial [] = null_animation
serial animations@(first:rest) = Animation{..}
  where
    animationDuration = sum . map durationOf $ animations
    animationCache = AnimationZipper{..}
      where
        zipperLeft = []
        zipperRight = rest
        zipperCurrent = first
        zipperLeftTime = 0

    animationFunction time state zipper@AnimationZipper{..}
      -- Clamp the time to zero from below.
      | time < 0 =
          animationFunction 0 state zipper
      -- Clamp the time to the sum of the start and duration of the
      -- last animation from above.
      | time > zipperLeftTime + durationOf zipperCurrent && null zipperRight =
          animationFunction (zipperLeftTime + durationOf zipperCurrent) state zipper
      | time < zipperLeftTime =
          assert (not . null $ zipperLeft) $ -- internal invariant
          uncurry (animationFunction time) (moveLeft state zipper)
      | time >= zipperLeftTime + durationOf zipperCurrent && (not . null) zipperRight =
          assert (not . null $ zipperRight) $ -- internal invariant
          uncurry (animationFunction time) (moveRight state zipper)
      | otherwise = (new_state, new_zipper)
      where
          (new_state, new_animation) =
              runAnimation zipperCurrent (time - zipperLeftTime) state
          new_zipper = zipper{zipperCurrent=new_animation}

type ParallelCache α β = [Animation α β]

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
                in runAnimation animation clamped_time state
            )
            state
            list

