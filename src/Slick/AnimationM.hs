{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.AnimationM where

import Control.Applicative ((<$>),(<*>))
import Control.Lens (Lens',(&),(^.),(.~),(.=),(%=),(<%=),set,use)
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.State (State,get,execState, put,runState)

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

type Timelike t = (Fractional t, Ord t)

combineAnimationsUsing :: Timelike t ⇒ CombinationMode → [Animation t s] → Animation t s
combineAnimationsUsing Serial = serial
combineAnimationsUsing Parallel = parallel

combineTimesUsing :: Timelike t ⇒ CombinationMode → t → t → t
combineTimesUsing Serial = (+)
combineTimesUsing Parallel = curry fst

updateTimeWithDuration :: Timelike t ⇒ t → AnimationM t s t
updateTimeWithDuration duration = do
    old_time ← use ams_time
    updateTimeWithTime (old_time + duration)

updateTimeWithTime :: Timelike t ⇒ t → AnimationM t s t
updateTimeWithTime new = do
    combination_mode ← use ams_combination_mode
    ams_time <%= flip (combineTimesUsing combination_mode) new

combineDurationsUsing :: Timelike t ⇒ CombinationMode → t → t → t
combineDurationsUsing Serial = (+)
combineDurationsUsing Parallel = max

updateDuration :: Timelike t ⇒ t → AnimationM t s t
updateDuration new = do
    combination_mode ← use ams_combination_mode
    ams_duration <%= flip (combineDurationsUsing combination_mode) new

updateTimeAndDurationWithDuration :: Timelike t ⇒ t → AnimationM t s (t,t)
updateTimeAndDurationWithDuration new =
    (,) <$> updateDuration new <*> updateTimeWithDuration new

type AnimationM t s = State (AnimationMState t s)

within :: Timelike t ⇒ Lens' s s' → AnimationM t s' α → AnimationM t s α
within alens action = do
    old_state ← get
    let (result,new_state) =
            runState
                action
                (old_state{
                    _ams_state=old_state ^. ams_state . alens,
                    _ams_animations=DList.empty
                })
    ams_duration .= new_state ^. ams_duration
    ams_time .= new_state ^. ams_time
    ams_state %= set alens (new_state ^. ams_state)
    ams_animations %= (flip DList.append $ fmap (promoteAnimation alens) (new_state ^. ams_animations))
    return result

appendAnimation :: Animation t s → AnimationM t s ()
appendAnimation = (ams_animations %=) . flip DList.snoc

runAnimationMIn :: Timelike t ⇒ CombinationMode → AnimationM t s () → s → Animation t s
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

