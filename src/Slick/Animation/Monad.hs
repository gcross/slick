{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation.Monad where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.State.Strict (StateT(..),evalStateT,execStateT,get,put)

import Slick.Animation

data AnimationState t s = AnimationState
    {   _a_animation :: Animation t s
    ,   _a_state :: s
    }
makeLenses ''AnimationState

newtype AnimationM t s m α = AnimationM { unwrapAnimationM :: StateT (AnimationState t s) m α }

startAnimationM :: Monad m ⇒ Animation t s → s → AnimationM t s m ()
startAnimationM animation state = AnimationM . put $ AnimationState animation state

runAt :: Monad m ⇒ t → AnimationM t s m s
runAt t = AnimationM $ do
    animation_state ← get
    let (new_state, new_animation) =
            runAnimation
                (animation_state ^. a_animation)
                t
                (animation_state ^. a_state)
    put $ AnimationState new_animation new_state
    return new_state

runAnimationM :: Monad m ⇒ AnimationM t s m α → AnimationState t s → m (α, AnimationState t s)
runAnimationM = runStateT . unwrapAnimationM

evalanimationm :: Monad m ⇒ AnimationM t s m α → AnimationState t s → m α
evalanimationm = evalStateT . unwrapAnimationM

execAnimationM :: Monad m ⇒ AnimationM t s m α → AnimationState t s → m (AnimationState t s)
execAnimationM = execStateT . unwrapAnimationM
