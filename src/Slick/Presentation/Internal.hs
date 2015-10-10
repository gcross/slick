{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Presentation.Internal where

import Control.Lens (Lens',(^.),(.=),(%=),use)
import Control.Lens.TH (makeLenses)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.State (State,get,runState,state)

import Data.Composition ((.**))
import Data.DList (DList)
import qualified Data.DList as DList

import Slick.Animation

data CombinationMode = Serial | Parallel deriving (Show, Eq)

data PresentationState t s = PresentationState
    { _p_combination_mode :: CombinationMode
    , _p_duration :: t
    , _p_time :: t
    , _p_state :: s
    , _p_animations :: DList (Animation t s)
    }
makeLenses ''PresentationState

newtype Presentation t s α = Presentation {unwrapPresentation :: InnerPresentation t s α}
  deriving (Applicative,Functor,Monad)

type InnerPresentation t s = State (PresentationState t s)

instance Timelike t ⇒ State.MonadState s (Presentation t s) where
    get = Presentation $ use p_state
    put s = Presentation $ do
        old_s ← use p_state
        appendAnimation $ instantaneousAnimation old_s s
        p_state .= s
    state act = Presentation $ state act'
      where
        act' old_state = (value, new_state)
          where
            old_state' = old_state ^. p_state
            (value, new_state') = act old_state'
            new_state = PresentationState Serial 0 0 new_state' (DList.singleton $ instantaneousAnimation old_state' new_state')

combineAnimationsUsing :: Timelike t ⇒ CombinationMode → [Animation t s] → Animation t s
combineAnimationsUsing Serial = serial
combineAnimationsUsing Parallel = parallel

appendAnimation :: Timelike t ⇒ Animation t s → InnerPresentation t s ()
appendAnimation animation = do
    combination_mode ← use p_combination_mode
    let animation_duration = durationOf animation
    p_duration %=
        case combination_mode of
            Serial → (+) animation_duration
            Parallel → max animation_duration
    p_time %= (+ animation_duration)
    p_state %= fst . runAnimation animation animation_duration
    p_animations %= flip DList.snoc animation

runPresentationIn :: Timelike t ⇒ CombinationMode → s → InnerPresentation t s α → (α, AnimationAndState t s)
runPresentationIn combination_mode initial_state action = (final_value, AnimationAndState animation initial_state)
  where
    (final_value,final_animation_state) =
        runState action $
            PresentationState
                combination_mode
                0
                0
                initial_state
                DList.empty
    animation =
        combineAnimationsUsing
            combination_mode
            (DList.toList $ final_animation_state ^. p_animations)

execPresentationIn :: Timelike t ⇒ CombinationMode → s → InnerPresentation t s α → AnimationAndState t s
execPresentationIn = snd .** runPresentationIn

execPresentationIn' :: Timelike t ⇒ CombinationMode → s → InnerPresentation t s α → Animation t s
execPresentationIn' = (^. as_animation) .** execPresentationIn

within :: Timelike t ⇒ Lens' s s' → InnerPresentation t s' α → InnerPresentation t s α
within lens action = do
    old_state ← get
    let (result,new_state) =
            runState
                action
                (old_state{
                    _p_state=old_state ^. p_state . lens,
                    _p_animations=DList.empty
                })
    appendAnimation . promoteAnimation lens $
        combineAnimationsUsing (old_state ^. p_combination_mode) (DList.toList $ new_state ^. p_animations)
    return result

in_ :: Timelike t ⇒ CombinationMode → InnerPresentation t s α → InnerPresentation t s α
in_ combination_mode action = do
    old_state ← get
    let (result,new_state) =
            runState
                action
                (old_state{
                    _p_combination_mode=combination_mode,
                    _p_duration=0,
                    _p_state=old_state ^. p_state,
                    _p_animations=DList.empty
                })
    appendAnimation $
        combineAnimationsUsing combination_mode (DList.toList $ new_state ^. p_animations)
    return result
