{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.AnimationM.Internal where

import Control.Applicative ((<$>),(<*>))
import Control.Lens (Lens',(&),(^.),(.~),(.=),(%=),(<%=),set,use,view)
import Control.Lens.TH (makeLenses)
import qualified Control.Monad.State as State
import Control.Monad.Trans.State (State,get,execState, put,runState,state)

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor (fmap)
import Data.Time.Clock (DiffTime)

import Slick.Animation

data CombinationMode = Serial | Parallel

data AnimationMState t s = AnimationMState
    { _ams_combination_mode :: CombinationMode
    , _ams_duration :: t
    , _ams_time :: t
    , _ams_state :: s
    , _ams_animations :: DList (Animation t s)
    }
makeLenses ''AnimationMState

newtype AnimationM t s α = AnimationM {unwrapAnimationM :: InnerAnimationM t s α}
  deriving (Applicative,Functor,Monad)

type InnerAnimationM t s = State (AnimationMState t s)

instance Num t ⇒ State.MonadState s (AnimationM t s) where
    get = AnimationM $ use ams_state
    put s = AnimationM $ ams_state .= s
    state act = AnimationM $ state act'
      where
        act' old_state = (value, new_state)
          where
            (value, new_state') = act (old_state ^. ams_state)
            new_state = AnimationMState Serial 0 0 new_state' DList.empty

type Timelike t = (Fractional t, Ord t)

combineAnimationsUsing :: Timelike t ⇒ CombinationMode → [Animation t s] → Animation t s
combineAnimationsUsing Serial = serial
combineAnimationsUsing Parallel = parallel

appendAnimation :: Timelike t ⇒ Animation t s → InnerAnimationM t s ()
appendAnimation animation = do
    combination_mode ← use ams_combination_mode
    let animation_duration = durationOf animation
    ams_duration %=
        case combination_mode of
            Serial → (+) animation_duration
            Parallel → max animation_duration
    ams_time %= (+ animation_duration)
    ams_state %= fst . (runAnimation animation animation_duration)
    ams_animations %= (flip DList.snoc animation)

runAnimationMIn :: Timelike t ⇒ CombinationMode → InnerAnimationM t s () → s → Animation t s
runAnimationMIn combination_mode action initial_state = animation
  where
    (_,final_animation_state) =
        runState action $
            AnimationMState
                combination_mode
                0
                0
                initial_state
                DList.empty
    animation =
        combineAnimationsUsing
            combination_mode
            (DList.toList $ final_animation_state ^. ams_animations)

within :: Timelike t ⇒ Lens' s s' → InnerAnimationM t s' α → InnerAnimationM t s α
within lens action = do
    old_state ← get
    let (result,new_state) =
            runState
                action
                (old_state{
                    _ams_state=old_state ^. ams_state . lens,
                    _ams_animations=DList.empty
                })
    appendAnimation . promoteAnimation lens $
        combineAnimationsUsing (old_state ^. ams_combination_mode) (DList.toList $ new_state ^. ams_animations)
    return result

