{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Renderer.Rasterific.SDL where

import Prelude hiding (init)

import Data.Bits ((.|.))
import Data.Vector.Storable (unsafeWith)

import Foreign.C.String (peekCString,withCString)
import Foreign.Marshal (alloca)
import Foreign.Ptr (castPtr,nullPtr)
import Foreign.Storable (peek)

import Graphics.UI.SDL
    (Event(..)
    ,createRGBSurfaceFrom
    ,createRenderer
    ,createTextureFromSurface
    ,createWindow
    ,destroyTexture
    ,freeSurface
    ,getError
    ,init
    ,pollEvent
    ,quit
    ,renderClear
    ,renderCopy
    ,renderPresent
    ,setWindowBordered
    ,setWindowSize
    )
import Graphics.UI.SDL.Enum

import Slick.Renderer.Common (fixSize)
import Slick.Renderer.Rasterific (Graphic(..),renderGraphic)

errorWhen :: String → Bool → IO ()
errorWhen _ False = return ()
errorWhen label True = do
    msg ← getError >>= peekCString
    error $ label ++ " error: " ++ msg
    quit

mainViewing :: Graphic → IO ()
mainViewing graphic@Graphic{..} = do
    -- Compute the initial width and height of the window.
    let initial_width, initial_height :: Int
        initial_width = floor graphicWidth
        initial_height = floor graphicHeight

    -- Initialize SDL.
    retcode ← init SDL_INIT_VIDEO
    errorWhen "Initialization" (retcode /= 0)

    -- Create the window.
    window ← withCString "Hello, world!" $ \title →
        createWindow
            title
            0 0
            (fromIntegral initial_width) (fromIntegral initial_height)
            (SDL_WINDOW_RESIZABLE .|. SDL_WINDOW_SHOWN)
    errorWhen "Create window" (window == nullPtr)

    setWindowBordered window True

    -- Create the renderer.
    renderer ← createRenderer window (-1) SDL_RENDERER_ACCELERATED
    errorWhen "Create renderer" (renderer == nullPtr)

    -- Now we can define renderImage

    let renderImage :: Int → Int → IO ()
        renderImage width height = do
            -- Render the graphic into an image.
            let vec = renderGraphic graphic width height

            -- Create a surface with the image data.
            surface ← unsafeWith vec $ \ptr →
                createRGBSurfaceFrom
                    (castPtr ptr)
                    (fromIntegral width) (fromIntegral height)
                    32
                    (4*fromIntegral width)
                    0 0 0 0
            errorWhen "Create surface" (surface == nullPtr)

            -- Copy the surface to a texture, and then delete the surface.
            texture ← createTextureFromSurface renderer surface
            errorWhen "Create texture" (surface == nullPtr)
            freeSurface surface

            -- Clear the renderer
            retcode ← renderClear renderer
            errorWhen "Clear renderer" (retcode /= 0)

            -- Copy the texture into the renderer, and delete the texture.
            retcode ← renderCopy renderer texture nullPtr nullPtr
            errorWhen "Copy renderer" (retcode /= 0)
            destroyTexture texture

            -- Present the new state of the renderer on the screen.
            renderPresent renderer

    -- Render the image to the screen with the initial width and height.
    renderImage initial_width initial_height

    -- Loop through the event queue.
    let go = do
            event ← alloca $ \p_event → pollEvent p_event >> peek p_event
            case event of
                WindowEvent {..} → do
                    case windowEventEvent of
                        SDL_WINDOWEVENT_CLOSE → return ()
                        SDL_WINDOWEVENT_RESIZED → do
                            -- We only let the window be resized to a width and height
                            -- with the aspect ratio of the original image.
                            let width = windowEventData1
                                height = windowEventData2
                                (fixed_width, fixed_height) = fixSize 2 width height
                            setWindowSize window fixed_width fixed_height
                            renderImage (fromIntegral fixed_width) (fromIntegral fixed_height)
                            go
                        _ → go
                _ → go
    go
