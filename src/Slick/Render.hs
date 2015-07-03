{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Slick.Render where

import Control.Monad (forever,void)
import Control.Monad.IO.Class (liftIO)

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.Conduit (Consumer, (=$=), await, runConduit)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CChar(..), CInt(..), CULong(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL
    (Event(..)
    ,Renderer
    ,Window
    ,addTimer
    ,createRGBSurfaceFrom
    ,createRenderer
    ,createTextureFromSurface
    ,createWindow
    ,destroyTexture
    ,freeSurface
    ,getError
    ,hasEvents
    ,keysymKeycode
    ,mkTimerCallback
    ,pushEvent
    ,renderClear
    ,renderCopy
    ,renderPresent
    ,pollEvent
    ,pumpEvents
    ,quit
    ,setWindowBordered
    ,setWindowSize
    )

import Control.Lens ((^.))
import Control.Monad (forever,when)

import Data.Time.Clock (UTCTime)
import Data.IORef (readIORef,newIORef,writeIORef)

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke)

import qualified Text.XML as XML
import Text.XML (Document(..), Element(..), Name(..), renderBytes)

import System.Exit

import Unsafe.Coerce (unsafeCoerce)

import Slick.Animation
import Slick.SVG

newtype CairoContext = CairoContext (Ptr ())
newtype CairoSurface = CairoSurface (Ptr ())
newtype RsvgError = RsvgError (Ptr (Ptr CChar))
newtype RsvgHandle = RsvgHandle (Ptr ())

ignore_rsvg_error = RsvgError nullPtr

foreign import ccall "rsvg_handle_new" c_rsvg_handle_new :: IO RsvgHandle
foreign import ccall "rsvg_handle_new_from_file" c_rsvg_handle_new_from_file :: CString → RsvgError → IO RsvgHandle
foreign import ccall "rsvg_handle_write" c_rsvg_handle_write :: RsvgHandle → Ptr CChar → CULong → RsvgError → IO Bool
foreign import ccall "rsvg_handle_close" c_rsvg_handle_close :: RsvgHandle → IO Bool
foreign import ccall "rsvg_handle_render_cairo" c_rsvg_handle_render_cairo :: RsvgHandle → CairoContext → IO Bool

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create :: CInt → CInt → CInt → IO CairoSurface
foreign import ccall "cairo_image_surface_create_for_data" c_cairo_image_surface_create_for_data :: Ptr () → CInt → CInt → CInt → IO (CairoSurface)
foreign import ccall "cairo_create" c_cairo_create :: CairoSurface → IO CairoContext
foreign import ccall "cairo_destroy" c_cairo_destroy :: CairoContext → IO ()
foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data :: CairoSurface → IO (Ptr ())
foreign import ccall "cairo_paint" c_cairo_paint :: CairoContext → IO ()
foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb :: CairoContext → Double → Double → Double → IO ()

errorWhen _ False = return ()
errorWhen label True = do
    msg ← getError >>= peekCString
    error $ label ++ " error: " ++ msg
    quit

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

renderDocument :: Renderer → Document → IO ()
renderDocument renderer document = do
    let Header (round → width) (round → height) = document ^. header

    rsvg_handle ← c_rsvg_handle_new
    let consumer :: Consumer BS.ByteString IO ()
        consumer = do
            mbs ← await
            case mbs of
                Nothing → void . liftIO $ c_rsvg_handle_close rsvg_handle
                Just bs →
                    (liftIO . BS.useAsCString bs $ \ptr →
                        c_rsvg_handle_write rsvg_handle ptr (fromIntegral $ BS.length bs) ignore_rsvg_error)
                    >>
                    consumer
    runConduit $ renderBytes def document =$= consumer
    c_rsvg_handle_close rsvg_handle

    image_surface ← c_cairo_image_surface_create 1 (fromIntegral width) (fromIntegral height)
    cairo_context ← c_cairo_create image_surface
    c_cairo_set_source_rgb cairo_context 1 1 1
    c_cairo_paint cairo_context
    c_rsvg_handle_render_cairo rsvg_handle cairo_context

    image_surface_ptr ← c_cairo_image_surface_get_data image_surface
    surface ← createRGBSurfaceFrom image_surface_ptr (fromIntegral width) (fromIntegral height) 32 (4*fromIntegral width) 0 0 0 0

    texture ← createTextureFromSurface renderer surface
    errorWhen "Create texture" (surface == nullPtr)
    freeSurface surface
    c_cairo_destroy cairo_context

    retcode ← renderCopy renderer texture nullPtr nullPtr
    errorWhen "Copy renderer" (retcode /= 0)
    destroyTexture texture

    renderPresent renderer

withSDL :: Int → Int → (Window → Renderer → IO α) → IO α
withSDL initial_width initial_height action = do
    retcode ← SDL.init (SDL.SDL_INIT_EVERYTHING)
    errorWhen "Init" (retcode /= 0)

    window ← withCString "Hello, world!" $ \title →
        createWindow
            title
            100 100
            (fromIntegral initial_width)
            (fromIntegral initial_height)
            (SDL.SDL_WINDOW_SHOWN .|. SDL.SDL_WINDOW_RESIZABLE)
    errorWhen "Create window" (window == nullPtr)

    setWindowBordered window True
    renderer ← createRenderer window (-1) SDL.SDL_RENDERER_ACCELERATED
    errorWhen "Create renderer" (renderer == nullPtr)

    action_result ← action window renderer

    quit

    return action_result


viewDocument document@Document{..} = do
    let Element{..} = documentRoot
        Header (round → initial_width) (round → initial_height) = document ^. header
    let aspect_ratio :: Double
        aspect_ratio = fromIntegral initial_width / fromIntegral initial_height

    withSDL initial_width initial_height $ \window renderer → do

        renderDocument renderer document

        let go = do
                event ← alloca $ \p_event → pollEvent p_event >> peek p_event
                case event of
                    WindowEvent {..} → do
                        case windowEventEvent of
                            SDL.SDL_WINDOWEVENT_CLOSE → return ()
                            SDL.SDL_WINDOWEVENT_RESIZED → do
                                let width = windowEventData1
                                    height = windowEventData2
                                    (fixed_width, fixed_height) = fixSize aspect_ratio width height
                                setWindowSize window fixed_width fixed_height
                                renderDocument
                                    renderer
                                    $
                                    scaleDocument
                                        (fromIntegral fixed_width/fromIntegral initial_width)
                                        document
                                go
                            _ → go
                    _ → go
        go

data RunningStatus =
    Running !UTCTime !NominalDiffTime
  | Paused !NominalDiffTime

viewAnimation :: AnimationAndState NominalDiffTime s → (s → Document) → IO ()
viewAnimation animation_and_state render = do
    let animation_and_state_at_0 = runAnimationAndState animation_and_state 0
        document_at_0@Document{..} = render (animation_and_state_at_0 ^. as_state)
        Header (round → initial_width) (round → initial_height) = document_at_0 ^. header
    let aspect_ratio :: Double
        aspect_ratio = fromIntegral initial_width / fromIntegral initial_height

    starting_time ← getCurrentTime
    running_status_ref ← newIORef $ Running starting_time 0

    timer_callback2 ← mkTimerCallback $ \interval _ → return interval

    withSDL initial_width initial_height $ \window renderer → do

        renderDocument renderer document_at_0

        animation_and_state_ref ← newIORef animation_and_state_at_0
        scale_ref ← newIORef 1

        let pause = do
                running_status ← readIORef running_status_ref
                case running_status of
                    Paused _ → return ()
                    Running starting_time additional_time → do
                        current_time ← getCurrentTime
                        writeIORef running_status_ref $
                            Paused ((current_time `diffUTCTime` starting_time) + additional_time)
            resume = do
                running_status ← readIORef running_status_ref
                case running_status of
                    Running _ _ → return ()
                    Paused additional_time → do
                        current_time ← getCurrentTime
                        writeIORef running_status_ref $ Running current_time additional_time
            toggle = do
                running_status ← readIORef running_status_ref
                case running_status of
                    Paused _ → putStrLn "Resuming..." >> resume
                    Running _ _ → putStrLn "Pausing..." >> pause
            redraw = do
                time ← do
                    running_status ← readIORef running_status_ref
                    case running_status of
                        Running starting_time additional_time → do
                            current_time ← getCurrentTime
                            return $ (current_time `diffUTCTime` starting_time) + additional_time
                        Paused additional_time → return additional_time
                new_state ← runAnimationAndStateInIORef animation_and_state_ref time
                let document = render new_state
                scale ← readIORef scale_ref
                renderDocument renderer (scaleDocument scale document)
            processEvent = do
                pumpEvents
                has_events ← hasEvents SDL.SDL_WINDOWEVENT SDL.SDL_KEYDOWN
                when has_events $ do
                    event ← alloca $ \p_event → pollEvent p_event >> peek p_event
                    putStrLn $ "Next event is " ++ show event
                    case event of
                        WindowEvent {..} →
                            case windowEventEvent of
                                SDL.SDL_WINDOWEVENT_CLOSE → exitSuccess
                                SDL.SDL_WINDOWEVENT_EXPOSED → return ()
                                SDL.SDL_WINDOWEVENT_RESIZED → do
                                    let width = windowEventData1
                                        height = windowEventData2
                                        (fixed_width, fixed_height) = fixSize aspect_ratio width height
                                    setWindowSize window fixed_width fixed_height
                                    writeIORef scale_ref $ fromIntegral fixed_width/fromIntegral initial_width
                                    redraw
                                _ → return ()
                        KeyboardEvent {..} → putStrLn "Key detected!" >>
                            case keyboardEventState of
                                SDL.SDL_PRESSED →
                                    case keysymKeycode keyboardEventKeysym of
                                        SDL.SDLK_SPACE → toggle
                                        _ → return ()
                                _ → return ()
                        _ → return ()
                redraw
        forever $ processEvent
