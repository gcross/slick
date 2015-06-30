{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.Render where

import Control.Monad (forever,void)
import Control.Monad.IO.Class (liftIO)

import Data.Attoparsec.Text (Parser, choice, double, endOfInput, parseOnly, string)
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.Conduit (Consumer, (=$=), await, runConduit)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CChar(..), CInt(..), CULong(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL
    (Event(..)
    ,createRGBSurfaceFrom
    ,createRenderer
    ,createTextureFromSurface
    ,createWindow
    ,destroyTexture
    ,freeSurface
    ,getError
    ,renderClear
    ,renderCopy
    ,renderPresent
    ,pollEvent
    ,quit
    ,setWindowBordered
    ,setWindowSize
    )

import qualified Text.XML as XML
import Text.XML (Document(..), Element(..), Name(..), renderBytes)

import Unsafe.Coerce (unsafeCoerce)

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

size_parser :: Parser Double
size_parser =
    (*) <$> double
        <*> choice
                [string "pt" >> return 1.25
                ,string "pc" >> return 1.5
                ,string "mm" >> return 3.543307
                ,string "cm" >> return 35.43307
                ,string "in" >> return 90
                ,endOfInput >> return 1
                ]

viewDocument document@Document{..} = do
    let Element{..} = documentRoot
        parseDimension :: Integral α ⇒ Text → IO α
        parseDimension name =
            case parseOnly size_parser (fromMaybe (error $ unpack name ++" field does not exist.") $ Map.lookup (Name name Nothing Nothing) elementAttributes) of
                Left _ → error "Invalid format for the width attribute."
                Right result → return . fromIntegral . round $ result
    initial_width ← parseDimension "width"
    initial_height ← parseDimension "height"
    let aspect_ratio :: Double
        aspect_ratio = fromIntegral initial_width / fromIntegral initial_height
        current_transform = fromMaybe "" $ Map.lookup (Name "transform" Nothing Nothing) elementAttributes

    retcode ← SDL.init SDL.SDL_INIT_VIDEO
    errorWhen "Init" (retcode /= 0)

    window ← withCString "Hello, world!" $ \title →
        createWindow title 100 100 initial_width initial_height (SDL.SDL_WINDOW_SHOWN .|. SDL.SDL_WINDOW_RESIZABLE)
    errorWhen "Create window" (window == nullPtr)

    setWindowBordered window True
    renderer ← createRenderer window (-1) SDL.SDL_RENDERER_ACCELERATED
    errorWhen "Create renderer" (renderer == nullPtr)

    let renderImage :: Int → Int → IO ()
        renderImage width height = do
            let width' = fromIntegral width
                height' = fromIntegral height
                scale_factor :: Double
                scale_factor = fromIntegral width' / fromIntegral initial_width
                new_transformation = current_transform <> (pack $ "scale(" ++ show scale_factor ++ ")")
                render_document = document{documentRoot=documentRoot{elementAttributes=Map.insert "transform" new_transformation elementAttributes}}

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
            runConduit $ renderBytes def render_document =$= consumer
            c_rsvg_handle_close rsvg_handle

            image_surface ← c_cairo_image_surface_create 1 (fromIntegral width) (fromIntegral height)
            cairo_context ← c_cairo_create image_surface
            c_rsvg_handle_render_cairo rsvg_handle cairo_context
            c_cairo_destroy cairo_context

            image_surface_ptr ← c_cairo_image_surface_get_data image_surface
            surface ← createRGBSurfaceFrom image_surface_ptr (fromIntegral width) (fromIntegral height) 32 (4*fromIntegral width) 0 0 0 0

            texture ← createTextureFromSurface renderer surface
            errorWhen "Create texture" (surface == nullPtr)
            freeSurface surface

            SDL.setRenderDrawColor renderer 255 255 255 255

            retcode ← renderCopy renderer texture nullPtr nullPtr
            errorWhen "Copy renderer" (retcode /= 0)
            destroyTexture texture

            renderPresent renderer

    renderImage (fromIntegral initial_width) (fromIntegral initial_height)

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
                            renderImage (fromIntegral fixed_width) (fromIntegral fixed_height)
                            go
                        _ → go
                _ → go
    go

    quit
