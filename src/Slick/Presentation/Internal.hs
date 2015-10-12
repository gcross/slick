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
import qualified Data.Set as Set
import Data.Set (Set)

import Slick.Animation

data CombinationMode = Serial | Parallel deriving (Show, Eq)

data InnerPresentationMState t s = InnerPresentationMState
    { _ip_combination_mode :: CombinationMode
    , _ip_duration :: t
    , _ip_time :: t
    , _ip_state :: s
    , _ip_animations :: DList (Animation t s)
    , _ip_pauses :: Set t
    }
makeLenses ''InnerPresentationMState

newtype PresentationM t s α = PresentationM {unwrapPresentation :: InnerPresentationM t s α}
  deriving (Applicative,Functor,Monad)

type InnerPresentationM t s = State (InnerPresentationMState t s)

data Presentation t s = Presentation
    { _p_animation_and_state :: AnimationAndState t s
    , _p_pauses :: [t]
    }
makeLenses ''Presentation

instance Timelike t ⇒ State.MonadState s (PresentationM t s) where
    get = PresentationM $ use ip_state
    put s = PresentationM $ do
        old_s ← use ip_state
        appendAnimation $ instantaneousAnimation old_s s
        ip_state .= s
    state act = PresentationM $ state act'
      where
        act' old_state = (value, new_state)
          where
            old_state' = old_state ^. ip_state
            (value, new_state') = act old_state'
            new_state = InnerPresentationMState Serial 0 0 new_state' (DList.singleton $ instantaneousAnimation old_state' new_state') Set.empty

combineAnimationsUsing :: Timelike t ⇒ CombinationMode → [Animation t s] → Animation t s
combineAnimationsUsing Serial = serial
combineAnimationsUsing Parallel = parallel

appendAnimation :: Timelike t ⇒ Animation t s → InnerPresentationM t s ()
appendAnimation animation = do
    combination_mode ← use ip_combination_mode
    let animation_duration = durationOf animation
    ip_duration %=
        case combination_mode of
            Serial → (+) animation_duration
            Parallel → max animation_duration
    ip_time %= (+ animation_duration)
    ip_state %= fst . runAnimation animation animation_duration
    ip_animations %= flip DList.snoc animation

runPresentationIn :: Timelike t ⇒ CombinationMode → s → InnerPresentationM t s α → (α, Presentation t s)
runPresentationIn combination_mode initial_state action =
    (final_value, Presentation (AnimationAndState animation initial_state) pauses)
  where
    (final_value,final_presentation_state) =
        runState action $
            InnerPresentationMState
                combination_mode
                0
                0
                initial_state
                DList.empty
                Set.empty
    animation =
        combineAnimationsUsing
            combination_mode
            (DList.toList $ final_presentation_state ^. ip_animations)
    pauses = Set.toList . Set.insert (durationOf animation) . (^. ip_pauses) $ final_presentation_state

execPresentationIn :: Timelike t ⇒ CombinationMode → s → InnerPresentationM t s α → Presentation t s
execPresentationIn = snd .** runPresentationIn

within :: Timelike t ⇒ Lens' s s' → InnerPresentationM t s' α → InnerPresentationM t s α
within lens action = do
    old_state ← get
    let (result,new_state) =
            runState
                action
                (old_state{
                    _ip_state=old_state ^. ip_state . lens,
                    _ip_animations=DList.empty
                })
    appendAnimation . promoteAnimation lens $
        combineAnimationsUsing (old_state ^. ip_combination_mode) (DList.toList $ new_state ^. ip_animations)
    return result

in_ :: Timelike t ⇒ CombinationMode → InnerPresentationM t s α → InnerPresentationM t s α
in_ combination_mode action = do
    old_state ← get
    let (result,new_state) =
            runState
                action
                (old_state{
                    _ip_combination_mode=combination_mode,
                    _ip_duration=0,
                    _ip_state=old_state ^. ip_state,
                    _ip_animations=DList.empty
                })
    appendAnimation $
        combineAnimationsUsing combination_mode (DList.toList $ new_state ^. ip_animations)
    return result
