{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import Data.Conduit (Consumer, (=$=), await, runConduit)
import Data.Default (def)

import Foreign.C.String (CString)
import Foreign.C.Types (CULong(..))
import Foreign.Ptr (Ptr)

import qualified Text.XML as XML

foreign import ccall "slick_write_to_handle" c_slick_write_to_handle :: Ptr () → CString → CULong → IO ()
foreign import ccall "slick_run" c_slick_run :: IO ()

foreign export ccall slick_write_document :: Ptr () → IO ()

slick_write_document :: Ptr () → IO ()
slick_write_document rsvg_handle = do
    document ← XML.readFile def "quantum-mechanic.svg"
    let consumer :: Consumer BS.ByteString IO ()
        consumer = do
            mbs ← await
            case mbs of
                Nothing → return ()
                Just bs → do
                    (liftIO . BS.useAsCString bs $ \ptr →
                        c_slick_write_to_handle rsvg_handle ptr (fromIntegral $ BS.length bs))
                    consumer
    runConduit $ XML.renderBytes def document =$= consumer

main = c_slick_run
