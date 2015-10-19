{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation.Monad where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.State.Strict (StateT(..),evalStateT,execStateT,get,put)

import Slick.Animation

data AnimationState s = AnimationState
    {   _a_animation :: Animation s
    ,   _a_state :: s
    }
makeLenses ''AnimationState

newtype AnimationM s m α = AnimationM { unwrapAnimationM :: StateT (AnimationState s) m α }

startAnimationM :: Monad m ⇒ Animation s → s → AnimationM s m ()
startAnimationM animation state = AnimationM . put $ AnimationState animation state

runAt :: Monad m ⇒ Time → AnimationM s m s
runAt t = AnimationM $ do
    animation_state ← get
    let (new_state, new_animation) =
            runAnimation
                (animation_state ^. a_animation)
                t
                (animation_state ^. a_state)
    put $ AnimationState new_animation new_state
    return new_state

runAnimationM :: Monad m ⇒ AnimationM s m α → AnimationState s → m (α, AnimationState s)
runAnimationM = runStateT . unwrapAnimationM

evalAnimationM :: Monad m ⇒ AnimationM s m α → AnimationState s → m α
evalAnimationM = evalStateT . unwrapAnimationM

execAnimationM :: Monad m ⇒ AnimationM s m α → AnimationState s → m (AnimationState s)
execAnimationM = execStateT . unwrapAnimationM
