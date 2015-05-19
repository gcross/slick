{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Animation where

import Control.Exception (assert)
import Control.Lens (Iso',(^.),from,iso)

import Data.Composition ((.**))
import Data.List (mapAccumL)

data Animation t s = ∀ ɣ. Animation
    { animationDuration :: t
    , animationCache :: ɣ
    , animationFunction :: t → (s → ɣ → (s, ɣ))
    }

narrowingAnimation :: Iso' s s' → Iso' (Animation t s) (Animation t s')
narrowingAnimation aniso =
    iso (\case
          Animation{..} →
            let newAnimationFunction t s' c = (s_new ^. aniso,c_new)
                  where
                    (s_new,c_new) = animationFunction t (s' ^. from aniso) c
            in Animation animationDuration animationCache newAnimationFunction
        )
        (\case
          Animation{..} →
            let newAnimationFunction t s c = (s'_new ^. from aniso,c_new)
                  where
                    (s'_new,c_new) = animationFunction t (s ^. aniso) c
            in Animation animationDuration animationCache newAnimationFunction
        )

null_animation :: Num t ⇒ Animation t s
null_animation = Animation 0 () (\_ x y → (x,y))

cachelessAnimation :: t → (t → s → s) → Animation t s
cachelessAnimation duration function = Animation duration () (\t x () → (function t x, ()))

statelessAnimation :: t → (t → t) → Animation t t
statelessAnimation duration function = Animation duration () (\t _ () → (function t, ()))

durationOf :: Animation t s → t
durationOf animation = case animation of Animation{..} → animationDuration

runAnimation :: t → s → Animation t s → (s, Animation t s)
runAnimation t state Animation{..} =
    (new_state, Animation animationDuration new_cache animationFunction)
  where
    (new_state, new_cache) = animationFunction t state animationCache

runAnimationState :: t → s → Animation t s → s
runAnimationState = fst .** runAnimation

data Side = LeftSide | RightSide

serial [] = null_animation
serial [animation] = animation
serial animations = serial $ merge animations
  where
    merge [] = []
    merge [x] = [x]
    merge (x:y:rest) = Animation{..}:merge rest
      where
        duration_of_x = durationOf x

        animationDuration = duration_of_x + durationOf y
        animationCache = (x,y,LeftSide)
        animationFunction time state (x,y,last_side)
          | time < duration_of_x =
                let (new_state,new_x) = runAnimation time state x
                in (new_state,(new_x,y,LeftSide))
          | otherwise =
                case last_side of
                    LeftSide →
                        let (new_state,new_x) = runAnimation duration_of_x state x
                            (final_state,new_y) = runAnimation (time - durationOf x) new_state y
                        in (final_state,(new_x,new_y,RightSide))
                    RightSide →
                        let (new_state,new_y) = runAnimation (time - durationOf x) state y
                        in (new_state,(x,new_y,RightSide))

parallel :: (Num t, Ord t) ⇒ [Animation t s] → Animation t s
parallel [] = null_animation
parallel animations = Animation animationDuration animationCache animationFunction -- (Animation{..})
  where
    animationDuration = maximum . map durationOf $ animations

    animationCache = animations

    animationFunction time state list
      | time < 0 =
          animationFunction 0 state list
      | otherwise =
          mapAccumL
            (\state animation@Animation{..} →
                let clamped_time = if time > animationDuration then animationDuration else time
                in runAnimation clamped_time state animation
            )
            state
            list

