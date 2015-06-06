{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Presentation
    (Presentation
    ,CombinationMode(..)
    ,Timelike(..)
    ,appendAnimation
    ,runPresentationIn
    ,execPresentationIn
    ,execPresentationIn'
    ,within
    )
    where

import Control.Lens (Lens')

import Slick.Animation
import Slick.Presentation.Internal hiding
    (appendAnimation
    ,runPresentationIn
    ,execPresentationIn
    ,execPresentationIn'
    ,within
    )
import qualified Slick.Presentation.Internal as Internal

appendAnimation :: Timelike t ⇒ Animation t s → Presentation t s ()
appendAnimation = Presentation . Internal.appendAnimation

runPresentationIn :: Timelike t ⇒ CombinationMode → s → Presentation t s α → (α, AnimationAndState t s)
runPresentationIn combination_mode initial_state =
    Internal.runPresentationIn combination_mode initial_state . unwrapPresentation

execPresentationIn :: Timelike t ⇒ CombinationMode → s → Presentation t s α → AnimationAndState t s
execPresentationIn combination_mode initial_state =
    Internal.execPresentationIn combination_mode initial_state . unwrapPresentation

execPresentationIn' :: Timelike t ⇒ CombinationMode → s → Presentation t s α → Animation t s
execPresentationIn' combination_mode initial_state =
    Internal.execPresentationIn' combination_mode initial_state . unwrapPresentation

within :: Timelike t ⇒ Lens' s s' → Presentation t s' α → Presentation t s α
within lens (Presentation action) = Presentation $ Internal.within lens action
