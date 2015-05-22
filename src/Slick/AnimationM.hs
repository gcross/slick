{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.AnimationM where

import Control.Lens (Lens',(^.),set)
import Control.Monad.Trans.State (State,get,put,runState)

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor (fmap)
import Data.Time.Clock (DiffTime)

import Slick.Animation
import Slick.Interpolation

data AnimationMState t s = AnimationMState
    { animationMTime :: t
    , animationMState :: s
    , animationMAnimations :: DList (Animation t s)
    }

type AnimationM t s = State (AnimationMState t s)

within :: Lens' s s' → AnimationM t s' α → AnimationM t s α
within alens action = do
    old_state ← get
    let old_animation_state = animationMState old_state
        (result,new_state) = runState action (old_state{animationMState = old_animation_state ^. alens, animationMAnimations=DList.empty})
        new_animation_state = animationMState new_state
    put $ AnimationMState
        (animationMTime new_state)
        (set alens new_animation_state old_animation_state)
        (animationMAnimations old_state `DList.append` fmap (promoteAnimation alens) (animationMAnimations new_state))
    return result

easeTo :: ∀ t s s'. (Fractional t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → AnimationM t s ()
easeTo transition lens duration end = do
    AnimationMState old_duration old_state old_animations ← get
    let start = old_state ^. lens
        animation :: Animation t s
        animation = easeAnimation transition lens duration start end
        new_state = set lens end old_state
    put $ AnimationMState (old_duration+duration) new_state (old_animations `DList.snoc` animation)
