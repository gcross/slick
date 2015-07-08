{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Lens ((<%=),(^.),(.=),makeLenses,use)
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import Data.Conduit (Consumer, (=$=), await, runConduit)
import Data.Default (def)
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)

import Foreign.C.String (CString)
import Foreign.C.Types (CULong(..))
import Foreign.StablePtr
import Foreign.Ptr (Ptr)

import qualified Text.XML as XML
import Text.XML (Document)

import Slick.Animation
import Slick.Presentation
import Slick.SVG
import Slick.Transition

import qualified Data.ByteString.Lazy as LBS

import Debug.Trace

data Mode =
    RunMode UTCTime NominalDiffTime
  | PauseMode NominalDiffTime

data SlickState s = SlickState
    {   _s_canary :: String
    ,   _s_mode :: Mode
    ,   _s_animation_and_state :: AnimationAndState Double s
    ,   _s_renderer :: s → Document
    }
makeLenses ''SlickState

foreign import ccall "slick_write_to_handle" c_slick_write_to_handle :: Ptr () → CString → CULong → IO ()
foreign import ccall "slick_run" c_slick_run :: Ptr () → IO ()

withState :: Ptr () → StateT (SlickState s) IO α → IO α
withState state_ptr action = do
    putStrLn $ "STATE_PTR 2 =" ++ show state_ptr
    state_ref ← deRefStablePtr . castPtrToStablePtr $ state_ptr
    state ← readIORef state_ref
    putStrLn "BEFORE"
    putStrLn (state ^. s_canary)
    putStrLn "AFTER"
    (result, new_state) ← runStateT action state
    writeIORef state_ref new_state
    return result

foreign export ccall slick_write_document :: Ptr () → Ptr () → IO ()

slick_write_document :: Ptr () → Ptr () → IO ()
slick_write_document state_ptr rsvg_handle = withState state_ptr $ do
    liftIO $ putStrLn "ENTERED slick_write_document"
    use s_canary >>= liftIO . putStrLn . show
    mode ← use s_mode
    liftIO $ putStrLn "A"
    time ← liftIO $ case mode of
        RunMode starting_time additional_time → do
            current_time ← getCurrentTime
            return . realToFrac $ (current_time `diffUTCTime` starting_time) + additional_time
        PauseMode time → return . realToFrac $ time
    liftIO $ putStrLn "B"
    AnimationAndState _ new_state ← s_animation_and_state <%= runAnimationAndState time
    liftIO $ putStrLn "C"
    renderer ← use s_renderer
    liftIO $ putStrLn "D"
    let document = renderer new_state
    liftIO . putStrLn . show $ document
    let consumer = do
            mbs ← await
            case mbs of
                Nothing → return ()
                Just bs → do
                    (liftIO . BS.useAsCString bs $ \ptr →
                        c_slick_write_to_handle rsvg_handle ptr (fromIntegral $ BS.length bs))
                    consumer
    liftIO $ putStrLn "E"
    runConduit $ XML.renderBytes def document =$= consumer
    liftIO $ putStrLn "F"
    liftIO $ putStrLn "EXITED slick_write_document"

viewAnimation :: AnimationAndState Double s → (s → Document) → IO ()
viewAnimation animation_and_state render = do
    putStrLn "ENTERED viewAnimation"
    starting_time ← getCurrentTime
    state_ref ← newIORef $ SlickState "TEST" (RunMode starting_time 0) animation_and_state render
    state_ref_ptr ← newStablePtr state_ref
    putStrLn $ "STATE_PTR 1 =" ++ show (castStablePtrToPtr state_ref_ptr)
    c_slick_run . castStablePtrToPtr $ state_ref_ptr
    freeStablePtr state_ref_ptr

viewPresentation :: CombinationMode → s → (s → Document) → Presentation Double s () → IO ()
viewPresentation combination_mode initial_state render presentation =
    viewAnimation (execPresentationIn combination_mode initial_state presentation) render

data LogoState = LogoState
    {   _logo_the :: Use
    ,   _logo_uantum :: Use
    ,   _logo_mechanic :: Use
    ,   _logo_gear :: Use
    ,   _logo_gear_tail :: Use
    } deriving (Eq,Ord,Show)
makeLenses ''LogoState

main = do
    putStrLn "ENTERED MAIN"
    let filename = "quantum-mechanic.svg"
    document ← XML.readFile def filename
    let defs = mkDefsFromSVG document
        uses = extractElementsForUse document . Set.fromList $
            ["logo_the"
            ,"logo_gear"
            ,"logo_uantum"
            ,"logo_mechanic"
            ,"logo_gear"
            ,"logo_gear_tail"
            ]
        initial_logo_state = LogoState
            (fromJust $ Map.lookup "logo_the" uses)
            (fromJust $ Map.lookup "logo_uantum" uses)
            (fromJust $ Map.lookup "logo_mechanic" uses)
            (fromJust $ Map.lookup "logo_gear" uses)
            (fromJust $ Map.lookup "logo_gear_tail" uses)
        renderToDocument logo_state =
            trace "ENTERED renderToDocument" $
            svg (document ^. header)
                [defs
                ,renderUse $ logo_state ^. logo_the
                ,renderUse $ logo_state ^. logo_uantum
                ,renderUse $ logo_state ^. logo_mechanic
                ,renderUse $ logo_state ^. logo_gear
                ,renderUse $ logo_state ^. logo_gear_tail
                ]
    viewPresentation Serial initial_logo_state renderToDocument $ do
        logo_the . x .= -380
        logo_uantum . x .= 540
        logo_mechanic . y .= 280
        logo_gear . y .= -314
        logo_gear . rotation_x .= 450
        logo_gear . rotation_y .= 725
        logo_gear_tail . y .= -314

        in_ Parallel $ do
            decelerateTo (logo_the . x) 1 0
            decelerateTo (logo_uantum . x) 1 0
            decelerateTo (logo_mechanic . y) 1 0
            decelerateTo (logo_gear . y) 1 0
            decelerateTo (logo_gear_tail . y) 1 0
            decelerateBy (logo_gear . rotation_angle) 1 360
