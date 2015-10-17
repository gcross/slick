{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

type Duration = Double

type Time = Double

data Animation s = ∀ ɣ. Animation
    { animationDuration :: Duration
    , animationCache :: ɣ
    , animationFunction :: Time → s → ɣ → (s, ɣ)
    }

promoteAnimation :: Lens' s s' → Animation s' → Animation s
promoteAnimation alens Animation{..} =
    Animation animationDuration animationCache newAnimationFunction
  where
    newAnimationFunction t s c = (s_new,c_new)
      where
        (s'_new,c_new) = animationFunction t (s ^. alens) c
        s_new = set alens s'_new s

clampAnimation :: Animation s → Animation s
clampAnimation (Animation duration cache f) =
  Animation duration cache $ \t s c →
    if | t < 0 → f 0 s c
       | t > duration → f duration s c
       | otherwise → f t s c

null_animation :: Animation s
null_animation = Animation 0 () (\_ x y → (x,y))

cachelessAnimation :: Duration → (Time → s → s) → Animation s
cachelessAnimation duration function = clampAnimation $ Animation duration () (\t x () → (function t x, ()))

statelessAnimation :: Duration → (Time → Time) → Animation Time
statelessAnimation duration function = clampAnimation $ Animation duration () (\t _ () → (function t, ()))

constantAnimation :: Duration → Animation s
constantAnimation duration = cachelessAnimation duration (const id)

instantaneousAnimation :: s → s → Animation s
instantaneousAnimation before after = clampAnimation $ cachelessAnimation 0.00000001 (\t _ → if t == 0 then before else after)

durationOf :: Animation s → Duration
durationOf animation = case animation of Animation{..} → animationDuration

runAnimation :: Animation s → Time → s → (s, Animation s)
runAnimation Animation{..} t state =
    (new_state, Animation animationDuration new_cache animationFunction)
  where
    (new_state, new_cache) = animationFunction t state animationCache

execAnimation :: Animation s → Time → s → s
execAnimation = fst .** runAnimation

data AnimationAndState s = AnimationAndState
    { _as_animation :: Animation s
    , _as_state :: s
    }
makeLenses ''AnimationAndState

runAnimationAndState :: Time → AnimationAndState s → AnimationAndState s
runAnimationAndState t old = AnimationAndState new_animation new_state
  where
    (new_state, new_animation) = runAnimation (old ^. as_animation) t (old ^. as_state)

runAnimationAndStateInIORef :: Time → IORef (AnimationAndState s) → IO s
runAnimationAndStateInIORef t ref = do
    old ← readIORef ref
    let new = runAnimationAndState t old
    writeIORef ref new
    return (new ^. as_state)

data AnimationZipper s = AnimationZipper
    { zipperLeft :: [Animation s]
    , zipperRight :: [Animation s]
    , zipperCurrent :: Animation s
    , zipperLeftTime :: Time
    }

moveLeft :: s → AnimationZipper s → (s, AnimationZipper s)
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

moveRight :: s → AnimationZipper s → (s, AnimationZipper s)
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

serial :: [Animation s] → Animation s
serial [] = null_animation
serial [animation] = animation
serial animations@(first:rest) = clampAnimation Animation{..}
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

parallel :: [Animation s] → Animation s
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
