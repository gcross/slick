{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.AnimationM
    (AnimationM
    ,CombinationMode(..)
    ,Timelike(..)
    ,appendAnimation
    ,runAnimationMIn
    ,within
    )
    where

import Control.Lens (Lens')

import Slick.Animation
import Slick.AnimationM.Internal hiding (appendAnimation,runAnimationMIn,within)
import qualified Slick.AnimationM.Internal as Internal

appendAnimation :: Timelike t ⇒ Animation t s → AnimationM t s ()
appendAnimation = AnimationM . Internal.appendAnimation

runAnimationMIn :: Timelike t ⇒ CombinationMode → s → AnimationM t s () → Animation t s
runAnimationMIn combination_mode initial_state =
    Internal.runAnimationMIn combination_mode initial_state . unwrapAnimationM

within :: Timelike t ⇒ Lens' s s' → AnimationM t s' α → AnimationM t s α
within lens (AnimationM action) = AnimationM $ Internal.within lens action
