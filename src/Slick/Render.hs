{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Slick.Render where

import Control.Lens ((%=),(<%=),makeLenses,use)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, runStateT)

import qualified Data.ByteString as BS
import Data.Conduit ((=$=), await, runConduit)
import Data.Default (def)
import Data.IORef
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)

import Foreign.C.String (CString)
import Foreign.C.Types (CULong(..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr

import qualified Text.XML as XML
import Text.XML (Document)

import Slick.Animation
import Slick.Presentation

foreign import ccall "slick_write_to_handle" c_slick_write_to_handle :: Ptr () → CString → CULong → IO ()
foreign import ccall "slick_run" c_slick_run :: Ptr () → IO ()

data Mode =
    RunMode UTCTime NominalDiffTime
  | PauseMode NominalDiffTime

data SlickState s = SlickState
    {   _s_mode :: Mode
    ,   _s_animation_and_state :: AnimationAndState Double s
    ,   _s_renderer :: s → Document
    }
makeLenses ''SlickState

fixSize :: (RealFrac α, Integral β, Integral ɣ) ⇒ α → β → β → (ɣ, ɣ)
fixSize correct_aspect_ratio width height = (fixed_width, fixed_height)
  where
    width_f = fromIntegral width
    height_f = fromIntegral height
    current_aspect_ratio = width_f/height_f
    fixed_width = round $
        if current_aspect_ratio > correct_aspect_ratio
        then width_f / current_aspect_ratio * correct_aspect_ratio
        else width_f
    fixed_height = round $
        if current_aspect_ratio < correct_aspect_ratio
        then height_f * current_aspect_ratio / correct_aspect_ratio
        else height_f

withState :: Ptr () → StateT (SlickState s) IO α → IO α
withState state_ptr action = do
    state_ref ← deRefStablePtr . castPtrToStablePtr $ state_ptr
    state ← readIORef state_ref
    (result, new_state) ← runStateT action state
    writeIORef state_ref new_state
    return result

foreign export ccall slick_write_document :: Ptr () → Ptr () → IO ()

slick_write_document :: Ptr () → Ptr () → IO ()
slick_write_document state_ptr rsvg_handle = withState state_ptr $ do
    mode ← use s_mode
    time ← liftIO $ case mode of
        RunMode starting_time additional_time → do
            current_time ← getCurrentTime
            return . realToFrac $ (current_time `diffUTCTime` starting_time) + additional_time
        PauseMode time → return . realToFrac $ time
    AnimationAndState _ new_state ← s_animation_and_state <%= runAnimationAndState time
    renderer ← use s_renderer
    let document = renderer new_state
        consumer = do
            mbs ← await
            case mbs of
                Nothing → return ()
                Just bs → do
                    (liftIO . BS.useAsCString bs $ \ptr →
                        c_slick_write_to_handle rsvg_handle ptr (fromIntegral $ BS.length bs))
                    consumer
    runConduit $ XML.renderBytes def document =$= consumer

foreign export ccall slick_toggle_mode :: Ptr () → IO ()

slick_toggle_mode :: Ptr () → IO ()
slick_toggle_mode state_ptr = withState state_ptr $ do
    current_time ← liftIO getCurrentTime
    s_mode %=
      (\mode →
        case mode of
            RunMode starting_time additional_time → PauseMode $ (current_time `diffUTCTime` starting_time) + additional_time
            PauseMode additional_time → RunMode current_time additional_time
      )

viewAnimation :: AnimationAndState Double s → (s → Document) → IO ()
viewAnimation animation_and_state render = do
    starting_time ← getCurrentTime
    state_ref ← newIORef $ SlickState (PauseMode 0.0000001) animation_and_state render
    state_ref_ptr ← newStablePtr state_ref
    c_slick_run . castStablePtrToPtr $ state_ref_ptr
    freeStablePtr state_ref_ptr

viewPresentation :: CombinationMode → s → (s → Document) → Presentation Double s () → IO ()
viewPresentation combination_mode initial_state render presentation =
    viewAnimation (execPresentationIn combination_mode initial_state presentation) render


