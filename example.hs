{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Lens (makeLenses,(^.),(.~))

import Data.Time.Clock(NominalDiffTime)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import System.Environment

import qualified Text.XML as XML

import Slick.Animation
import Slick.Render
import Slick.SVG

data LogoState = LogoState
    {   _logo_the :: Use
    ,   _logo_uantum :: Use
    ,   _logo_mechanic :: Use
    ,   _logo_gear :: Use
    ,   _logo_gear_tail :: Use
    }
makeLenses ''LogoState

main = do
    -- filename:_ ← getArgs
    let filename = "quantum-mechanic.svg"
    document ← XML.readFile def filename
    let defs = mkDefsFromSVG document
        uses = extractElementsForUse document . Set.fromList $
            ["logo_gear"
            ,"logo_the"
            ,"logo_uantum"
            ,"logo_gear_tail"
            ,"logo_mechanic"
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
                ,use $ logo_state ^. logo_the
                ,use $ logo_state ^. logo_uantum
                ,use $ logo_state ^. logo_mechanic
                ,use $ logo_state ^. logo_gear
                ,use $ logo_state ^. logo_gear_tail
                ]
        animation :: Animation NominalDiffTime LogoState
        animation = cachelessAnimation 100 (\t → logo_the . y .~ 100 * realToFrac t)
        animation_and_state = AnimationAndState animation initial_logo_state
    viewAnimation animation_and_state renderToDocument
