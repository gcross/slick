{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Lens (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State

import Data.IORef
import Data.Time.Clock

import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import Diagrams.Prelude hiding (Animation, radius, set, trace)

import Graphics.UI.Gtk hiding (Circle, get)

import Slick.Animation
import Slick.Presentation
import Slick.Transition


import Debug.Trace


data Circle = Circle
    {   _cx :: Double
    ,   _cy :: Double
    ,   _cr :: Double
    }
makeLenses ''Circle

main :: IO ()
main = do
    initial_time ← getCurrentTime
    initGUI
    window ← windowNew
    canvas ← drawingAreaNew
    canvas `on` sizeRequest $ return (Requisition 256 256)
    set window [ containerBorderWidth := 10,
                 containerChild := canvas ]
    animation_and_state_ref ← newIORef $
        (execPresentationIn Parallel (Circle 50 50 50) $ do
            in_ Serial $ do
                smoothByFactor cr 2 0.5
                in_ Parallel $ do
                    smoothBy cx 2 100
                    smoothBy cy 2 100
         :: AnimationAndState Double Circle)
    canvas `on` exposeEvent $ renderFigure animation_and_state_ref initial_time 
    onDestroy window mainQuit
    widgetShowAll window
    let tickHandler = widgetQueueDraw canvas >> return True
    timeoutAddFull tickHandler priorityDefaultIdle 30
    mainGUI

renderFigure :: IORef (AnimationAndState Double Circle) → UTCTime → EventM EExpose Bool
renderFigure animation_and_state_ref initial_time = do
    current_time ← liftIO getCurrentTime
    let dt :: NominalDiffTime
        dt = current_time `diffUTCTime` initial_time
        t :: Double
        t = fromRational . toRational $ dt
    current_radius ← liftIO $ runAnimationAndStateInIORef animation_and_state_ref t
    win ← eventWindow
    liftIO $ renderToGtk win $ figure current_radius
    return True

figure :: Circle → Diagram Cairo
figure c = unitCircle # scale (c ^. cr) # translateX (c ^. cx) # translateY (c ^. cy)
