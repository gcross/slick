{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation where

import Control.Exception (assert)
import Data.List (mapAccumL)

data Animation α β ɣ = Animation
    { animationDuration :: α
    , animationInitialCache :: ɣ
    , animationFunction :: α → (β → ɣ → (β, ɣ))
    }

data InactiveAnimation α β = ∀ ɣ. InactiveAnimation (Animation α β ɣ)

data ActiveAnimation α β = ∀ ɣ. ActiveAnimation (Animation α β ɣ) ɣ

data AnimationZipper α β = AnimationZipper
    { zipperLeft :: [InactiveAnimation α β]
    , zipperRight :: [InactiveAnimation α β]
    , zipperCurrent :: ActiveAnimation α β
    , zipperLeftTime :: α
    }

activateAnimation :: InactiveAnimation α β → ActiveAnimation α β
activateAnimation (InactiveAnimation animation) = ActiveAnimation animation (animationInitialCache animation)

deactivateAnimation :: ActiveAnimation α β → InactiveAnimation α β
deactivateAnimation (ActiveAnimation animation _) = InactiveAnimation animation

activeAnimationDuration (ActiveAnimation animation _) = animationDuration animation

inactiveAnimationDuration (InactiveAnimation animation) = animationDuration animation

moveLeft :: Num α ⇒ β → AnimationZipper α β → (β, AnimationZipper α β)
moveLeft state zipper@AnimationZipper{zipperLeft=[]} = (state, zipper)
moveLeft state (AnimationZipper (left:rest) right current left_time) = (new_state, new_zipper)
  where
    new_state = case current of
        (ActiveAnimation Animation{..} cache) → fst $ animationFunction left_time state cache
    new_zipper =
        AnimationZipper
        { zipperLeft = rest
        , zipperRight = deactivateAnimation current:right
        , zipperCurrent = activateAnimation left
        , zipperLeftTime = left_time - activeAnimationDuration current
        }

moveRight :: Num α ⇒ β → AnimationZipper α β → (β, AnimationZipper α β)
moveRight state zipper@AnimationZipper{zipperRight=[]} = (state, zipper)
moveRight state (AnimationZipper left (right:rest) current left_time) = (new_state, new_zipper)
  where
    new_state = case current of
        (ActiveAnimation Animation{..} cache) → fst $ animationFunction (left_time + animationDuration) state cache
    new_zipper =
        AnimationZipper
        { zipperLeft = deactivateAnimation current:left
        , zipperRight = rest
        , zipperCurrent = activateAnimation right
        , zipperLeftTime = left_time - activeAnimationDuration current
        }

null_animation :: Num α ⇒ InactiveAnimation α β
null_animation = InactiveAnimation (Animation 0 () (\_ x y → (x,y)))

serial :: (Num α, Ord α) ⇒ [InactiveAnimation α β] → InactiveAnimation α β
serial [] = null_animation
serial animations@(first:rest) = InactiveAnimation (Animation{..})
  where
    animationDuration = sum . map inactiveAnimationDuration $ animations

    animationInitialCache = AnimationZipper{..}
      where
        zipperLeft = []
        zipperRight = rest
        zipperCurrent = activateAnimation first
        zipperLeftTime = 0

    animationFunction time state zipper@AnimationZipper{..}
      | time < 0 =
          animationFunction 0 state zipper
      | time < zipperLeftTime =
          assert (not . null $ zipperLeft) $ -- internal invariant
          uncurry (animationFunction time) (moveLeft state zipper)
      | time >= zipperLeftTime + activeAnimationDuration zipperCurrent =
          assert (not . null $ zipperRight) $ -- internal invariant
          uncurry (animationFunction time) (moveRight state zipper)
      | otherwise =
          case zipperCurrent of
            ActiveAnimation (animation@Animation{..}) cache →
                let (new_state, new_cache) =
                        animationFunction (time - zipperLeftTime) state cache
                    new_zipper = zipper{zipperCurrent=ActiveAnimation animation new_cache}
                in (new_state, new_zipper)

type ParallelCache α β = [ActiveAnimation α β]

parallel :: (Num α, Ord α) ⇒ [InactiveAnimation α β] → InactiveAnimation α β
parallel [] = null_animation
parallel animations = InactiveAnimation (Animation animationDuration animationInitialCache animationFunction) -- (Animation{..})
  where
    animationDuration = maximum . map inactiveAnimationDuration $ animations

    animationInitialCache = map activateAnimation animations

    animationFunction time state list
      | time < 0 =
          animationFunction 0 state list
      | otherwise =
          mapAccumL
            (\state (ActiveAnimation (animation@Animation{..}) cache) →
                let clamped_time = if time > animationDuration then animationDuration else time
                    (new_state, new_cache) = animationFunction clamped_time state cache
                in (new_state, ActiveAnimation animation new_cache)
            )
            state
            list
