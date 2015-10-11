{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Presentation
    (PresentationM
    ,CombinationMode(..)
    ,Timelike
    ,appendAnimation
    ,runPresentationIn
    ,execPresentationIn
    ,within
    ,in_
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
    ,in_
    )
import qualified Slick.Presentation.Internal as Internal

appendAnimation :: Timelike t ⇒ Animation t s → PresentationM t s ()
appendAnimation = PresentationM . Internal.appendAnimation

runPresentationIn :: Timelike t ⇒ CombinationMode → s → PresentationM t s α → (α, AnimationAndState t s)
runPresentationIn combination_mode initial_state =
    Internal.runPresentationIn combination_mode initial_state . unwrapPresentation

execPresentationIn :: Timelike t ⇒ CombinationMode → s → PresentationM t s α → AnimationAndState t s
execPresentationIn combination_mode initial_state =
    Internal.execPresentationIn combination_mode initial_state . unwrapPresentation

within :: Timelike t ⇒ Lens' s s' → PresentationM t s' α → PresentationM t s α
within lens (PresentationM action) = PresentationM $ Internal.within lens action

in_ :: Timelike t ⇒ CombinationMode → PresentationM t s α → PresentationM t s α
in_ combination_mode (PresentationM action) = PresentationM $ Internal.in_ combination_mode action
