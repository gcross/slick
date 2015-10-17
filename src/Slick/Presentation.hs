{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Presentation
    (Presentation
    ,PresentationM
    ,CombinationMode(..)
    ,p_animation_and_state
    ,p_pauses
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

appendAnimation :: Animation s → PresentationM s ()
appendAnimation = PresentationM . Internal.appendAnimation

runPresentationIn :: CombinationMode → s → PresentationM s α → (α, Presentation s)
runPresentationIn combination_mode initial_state =
    Internal.runPresentationIn combination_mode initial_state . unwrapPresentation

execPresentationIn :: CombinationMode → s → PresentationM s α → Presentation  s
execPresentationIn combination_mode initial_state =
    Internal.execPresentationIn combination_mode initial_state . unwrapPresentation

within :: Lens' s s' → PresentationM s' α → PresentationM s α
within lens (PresentationM action) = PresentationM $ Internal.within lens action

in_ :: CombinationMode → PresentationM s α → PresentationM s α
in_ combination_mode (PresentationM action) = PresentationM $ Internal.in_ combination_mode action
