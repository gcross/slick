{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation where

import Control.Exception (assert)
import Control.Lens (Lens',(^.),makeLenses,set)

import Data.Composition ((.**))
import Data.List (mapAccumL)
import Data.IORef (IORef, readIORef, writeIORef)

data Animation t s = ∀ ɣ. Animation
    { animationDuration :: t
    , animationCache :: ɣ
    , animationFunction :: t → (s → ɣ → (s, ɣ))
    }

promoteAnimation :: Lens' s s' → Animation t s' → Animation t s
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

durationOf :: Animation t s → t
durationOf animation = case animation of Animation{..} → animationDuration

runAnimation :: Animation t s → t → s → (s, Animation t s)
runAnimation Animation{..} t state =
    (new_state, Animation animationDuration new_cache animationFunction)
  where
    (new_state, new_cache) = animationFunction t state animationCache

execAnimation :: Animation t s → t → s → s
execAnimation = fst .** runAnimation

data AnimationAndState t s = AnimationAndState
    { _as_animation :: Animation t s
    , _as_state :: s
    }
makeLenses ''AnimationAndState

runAnimationAndState :: AnimationAndState t s → t → AnimationAndState t s
runAnimationAndState old t = AnimationAndState new_animation new_state
  where
    (new_state, new_animation) = runAnimation (old ^. as_animation) t (old ^. as_state)

runAnimationAndStateInIORef :: IORef (AnimationAndState t s) → t → IO s
runAnimationAndStateInIORef ref t = do
    old ← readIORef ref
    let new = runAnimationAndState old t
    writeIORef ref new
    return (new ^. as_state)

data AnimationZipper t s = AnimationZipper
    { zipperLeft :: [Animation t s]
    , zipperRight :: [Animation t s]
    , zipperCurrent :: Animation t s
    , zipperLeftTime :: t
    }

moveLeft :: Num t ⇒ s → AnimationZipper t s → (s, AnimationZipper t s)
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
        , zipperLeftTime = left_time - durationOf left
        }

moveRight :: Num t ⇒ s → AnimationZipper t s → (s, AnimationZipper t s)
moveRight state zipper@AnimationZipper{zipperRight=[]} = (state, zipper)
moveRight state (AnimationZipper left (right:rest) current left_time) = (new_state, new_zipper)
  where
    new_state = case current of Animation{..} → fst $ animationFunction animationDuration state animationCache
    new_zipper =
        AnimationZipper
        { zipperLeft = current:left
        , zipperRight = rest
        , zipperCurrent = right
        , zipperLeftTime = left_time + durationOf current
        }

serial :: (Num t, Ord t) ⇒ [Animation t s] → Animation t s
serial [] = null_animation
serial animations@(first:rest) = clampAnimation $ Animation{..}
  where
    animationDuration = sum . map durationOf $ animations

    animationCache = AnimationZipper{..}
      where
        zipperLeft = []
        zipperRight = rest
        zipperCurrent = first
        zipperLeftTime = 0

    animationFunction time state zipper@AnimationZipper{..}
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

