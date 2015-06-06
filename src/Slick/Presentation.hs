{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Presentation
    (Presentation
    ,CombinationMode(..)
    ,Timelike(..)
    ,appendAnimation
    ,runPresentationIn
    ,runPresentationIn'
    ,within
    )
    where

import Control.Lens (Lens')

import Slick.Animation
import Slick.Presentation.Internal hiding
    (appendAnimation
    ,runPresentationIn
    ,runPresentationIn'
    ,within
    )
import qualified Slick.Presentation.Internal as Internal

appendAnimation :: Timelike t ⇒ Animation t s → Presentation t s ()
appendAnimation = Presentation . Internal.appendAnimation

runPresentationIn :: Timelike t ⇒ CombinationMode → s → Presentation t s () → Animation t s
runPresentationIn combination_mode initial_state =
    Internal.runPresentationIn combination_mode initial_state . unwrapPresentation

runPresentationIn' :: Timelike t ⇒ CombinationMode → s → Presentation t s () → AnimationAndState t s
runPresentationIn' combination_mode initial_state =
    Internal.runPresentationIn' combination_mode initial_state . unwrapPresentation

within :: Timelike t ⇒ Lens' s s' → Presentation t s' α → Presentation t s α
within lens (Presentation action) = Presentation $ Internal.within lens action
