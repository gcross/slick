{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Slick.Interpolation

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

within :: ∀ t s s' α. Timelike t ⇒ Lens' s s' → AnimationM t s' α → AnimationM t s α
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

easeFromTo :: ∀ t s s'. (Timelike t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → s' → AnimationM t s ()
easeFromTo transition lens duration start end = do
    let animation :: Animation t s
        animation = easeAnimation transition lens duration start end
        duration = durationOf animation
    appendAnimation animation
    ams_state %= set lens end
    updateTimeAndDurationWithDuration duration
    appendAnimation animation

easeTo :: (Timelike t, Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → AnimationM t s ()
easeTo transition lens duration end = do
    start ← use (ams_state . lens)
    easeFromTo transition lens duration start end

easeBy :: (Timelike t, Num s', Interpolatable t s') ⇒ (t → t) → Lens' s s' → t → s' → AnimationM t s ()
easeBy transition lens duration difference = do
    start ← use (ams_state . lens)
    let end = start + difference
    easeFromTo transition lens duration start end

in_ :: Timelike t ⇒ CombinationMode → AnimationM t s α → AnimationM t s α
in_ combination_mode action = do
    old_state ← get
    let starting_state =
            AnimationMState
                combination_mode
                (old_state ^. ams_duration)
                (old_state ^. ams_time)
                (old_state ^. ams_state)
                (DList.empty)
        (value,new_state) = runState action starting_state
        new_animation = combineAnimationsUsing combination_mode (DList.toList $ starting_state ^. ams_animations)
    updateTimeAndDurationWithDuration (durationOf new_animation)
    appendAnimation new_animation
    return value
