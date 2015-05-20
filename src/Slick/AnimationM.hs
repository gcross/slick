{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.AnimationM where

import Control.Lens (Lens',(^.),set)
import Control.Monad.Trans.State (State,get,put,runState)

import Data.Time.Clock (DiffTime)

import Slick.Animation

data AnimationMState t s = AnimationMState
    { animationMTime :: t
    , animationMState :: s
    , animationMAnimations :: [Animation t s]
    }

type AnimationM t s = State (AnimationMState t s)

within :: Lens' s s' → AnimationM t s' α → AnimationM t s α
within alens action = do
    old_state ← get
    let old_animation_state = animationMState old_state
        (result,new_state) = runState action (old_state{animationMState = old_animation_state ^. alens, animationMAnimations=[]})
        new_animation_state = animationMState new_state
    put $ AnimationMState
        (animationMTime new_state)
        (set alens new_animation_state old_animation_state)
        (animationMAnimations old_state ++ map (promoteAnimation alens) (animationMAnimations new_state))
    return result
