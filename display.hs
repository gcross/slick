{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Lens (makeLenses)
import Control.Monad.IO.Class (liftIO)

import Data.IORef
import Data.Time.Clock

import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import Diagrams.Prelude hiding (Animation, radius, set)

import Graphics.UI.Gtk hiding (Circle)

import Slick.Animation
import Slick.Presentation
import Slick.Transition

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
        (runPresentationIn' Serial 50 $ do
            -- smoothBy simple 2 50
            -- smoothBy simple 2 (-50)
            accelerateTo simple 2 (-50)
            decelerateTo simple 2 50
         :: AnimationAndState Double Double)
    canvas `on` exposeEvent $ renderFigure animation_and_state_ref initial_time 
    onDestroy window mainQuit
    widgetShowAll window
    let tickHandler = widgetQueueDraw canvas >> return True
    timeoutAddFull tickHandler priorityDefaultIdle 30
    mainGUI

renderFigure :: IORef (AnimationAndState Double Double) → UTCTime → EventM EExpose Bool
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

figure :: Double → Diagram Cairo
figure radius = unitCircle # scale 50 # translateY 50 # translateX radius
