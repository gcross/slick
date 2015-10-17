{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Slick.Render where

import Control.Lens ((^.),(%=),(<%=),(.=),makeLenses,use)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, runStateT)

import qualified Data.ByteString as BS
import Data.Conduit ((=$=), await, runConduit)
import Data.Default (def)
import Data.IORef
import qualified Data.List.PointedList as PointedList
import Data.List.PointedList (PointedList, focus, fromList, next, suffix)
import Data.Maybe (fromJust, fromMaybe)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)

import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..),CInt(..),CULong(..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr

import qualified Text.XML as XML
import Text.XML (Document)

import Slick.Animation
import Slick.Presentation
import Slick.SVG

foreign import ccall "slick_write_to_handle" c_slick_write_to_handle :: Ptr () → CString → CULong → IO ()
foreign import ccall "slick_run" c_slick_run :: CInt → CInt → Ptr () → IO ()

data Mode =
    RunMode UTCTime Time
  | PauseMode Time

data SlickState s = SlickState
    {   _s_mode :: Mode
    ,   _s_animation_and_state :: AnimationAndState s
    ,   _s_renderer :: Time → s → Document
    ,   _s_pause_zipper :: PointedList Time
    }
makeLenses ''SlickState

withState :: Ptr () → StateT (SlickState s) IO α → IO α
withState state_ptr action = do
    state_ref ← deRefStablePtr . castPtrToStablePtr $ state_ptr
    state ← readIORef state_ref
    (result, new_state) ← runStateT action state
    writeIORef state_ref new_state
    return result

foreign export ccall slick_write_document :: Ptr () → CDouble → Ptr () → IO ()

slick_write_document :: Ptr () → CDouble → Ptr () → IO ()
slick_write_document state_ptr scale rsvg_handle = withState state_ptr $ do
    mode ← use s_mode
    time ← case mode of
        PauseMode time → return time
        RunMode starting_time additional_time → do
            current_time ← liftIO getCurrentTime
            let time = realToFrac (current_time `diffUTCTime` starting_time) + additional_time
            next_pause_time ← use (s_pause_zipper . focus)
            if time >= next_pause_time
                then do
                    s_mode .= PauseMode next_pause_time
                    s_pause_zipper %= (\pause_zipper → fromMaybe pause_zipper (next pause_zipper))
                    return next_pause_time
                else
                    return time
    AnimationAndState _ new_state ← s_animation_and_state <%= runAnimationAndState time
    renderer ← use s_renderer
    let document = renderer (realToFrac scale) new_state
        consumer = do
            mbs ← await
            case mbs of
                Nothing → return ()
                Just bs → do
                    liftIO . BS.useAsCString bs $ \ptr →
                        c_slick_write_to_handle rsvg_handle ptr (fromIntegral $ BS.length bs)
                    consumer
    runConduit $ XML.renderBytes def document =$= consumer

foreign export ccall slick_toggle_mode :: Ptr () → IO ()

slick_toggle_mode :: Ptr () → IO ()
slick_toggle_mode state_ptr = withState state_ptr $ do
    current_time ← liftIO getCurrentTime
    s_mode %=
      (\mode →
        case mode of
            RunMode starting_time additional_time → PauseMode $ realToFrac (current_time `diffUTCTime` starting_time) + additional_time
            PauseMode additional_time → RunMode current_time additional_time
      )

viewAnimation :: Presentation s → (Time → s → Document) → IO ()
viewAnimation presentation render = do
    starting_time ← getCurrentTime
    let animation_and_state = presentation ^. p_animation_and_state
        next_pause = fromJust . fromList . (^. p_pauses) $ presentation
        initial_state = SlickState (PauseMode 0.0000001) animation_and_state render next_pause
        initial_document = render 1 $ animation_and_state ^. as_state
        Header initial_width initial_height = initial_document ^. header
    state_ref ← newIORef initial_state
    state_ref_ptr ← newStablePtr state_ref
    c_slick_run (round initial_width) (round initial_height) . castStablePtrToPtr $ state_ref_ptr
    freeStablePtr state_ref_ptr

viewPresentation :: CombinationMode → s → (Time → s → Document) → PresentationM s () → IO ()
viewPresentation combination_mode initial_state render presentation =
    viewAnimation (execPresentationIn combination_mode initial_state presentation) render


