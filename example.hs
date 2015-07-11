{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Lens ((.=),(%=),(^.),(.~),makeLenses,use)

import Data.Time.Clock(NominalDiffTime)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import System.Environment

import qualified Text.XML as XML

import Slick.Animation
import Slick.Presentation
import Slick.Render
import Slick.SVG
import Slick.Transition

import Control.Monad.State (get,put)
import Debug.Trace

data LogoState = LogoState
    {   _logo_the :: Actor
    ,   _logo_uantum :: Actor
    ,   _logo_mechanic :: Actor
    ,   _logo_gear :: Actor
    ,   _logo_gear_tail :: Actor
    } deriving (Eq,Ord,Show)
makeLenses ''LogoState

main = do
    -- filename:_ ← getArgs
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
