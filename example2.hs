{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Lens (makeLenses,use)
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import Data.Conduit (Consumer, (=$=), await, runConduit)
import Data.Default (def)
import Data.IORef

import Foreign.C.String (CString)
import Foreign.C.Types (CULong(..))
import Foreign.StablePtr
import Foreign.Ptr (Ptr)

import qualified Text.XML as XML

data SlickState = SlickState
    {   _s_document :: XML.Document
    }
makeLenses ''SlickState

foreign import ccall "slick_write_to_handle" c_slick_write_to_handle :: Ptr () → CString → CULong → IO ()
foreign import ccall "slick_run" c_slick_run :: Ptr () → IO ()

withState :: Ptr () → StateT SlickState IO α → IO α
withState state_ptr action = do
    state_ref ← deRefStablePtr . castPtrToStablePtr $ state_ptr
    state ← readIORef state_ref
    (result, new_state) ← runStateT action state
    writeIORef state_ref new_state
    return result

foreign export ccall slick_write_document :: Ptr () → Ptr () → IO ()

slick_write_document :: Ptr () → Ptr () → IO ()
slick_write_document state_ptr rsvg_handle = withState state_ptr $ do
    document ← use s_document
    let consumer = do
            mbs ← await
            case mbs of
                Nothing → return ()
                Just bs → do
                    (liftIO . BS.useAsCString bs $ \ptr →
                        c_slick_write_to_handle rsvg_handle ptr (fromIntegral $ BS.length bs))
                    consumer
    runConduit $ XML.renderBytes def document =$= consumer

main = do
    document ← XML.readFile def "quantum-mechanic.svg"
    state_ref ← newIORef (SlickState document)
    state_ref_ptr ← newStablePtr state_ref
    c_slick_run . castStablePtrToPtr $ state_ref_ptr
    freeStablePtr state_ref_ptr
