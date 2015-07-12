{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Lens (Lens',(^.),(.=),makeLenses)

import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import qualified Text.XML as XML

import Slick.Presentation
import Slick.Render
import Slick.SVG
import Slick.Transition

data LogoState = LogoState
    {   _actor_logo_the :: Actor
    ,   _actor_logo_uantum :: Actor
    ,   _actor_logo_mechanic :: Actor
    ,   _actor_logo_gear :: Actor
    ,   _actor_logo_gear_tail :: Actor
    } deriving (Eq,Ord,Show)
makeLenses ''LogoState

logo_the = actor_logo_the . attributes
logo_uantum = actor_logo_uantum . attributes
logo_mechanic = actor_logo_mechanic . attributes
logo_gear = actor_logo_gear . attributes
logo_gear_tail = actor_logo_gear_tail . attributes

main = do
    let filename = "quantum-mechanic.svg"
    document ← XML.readFile def filename
    let defs = mkDefsFromSVG document
        actor = extractActors document . Set.fromList $
            ["logo_the"
            ,"logo_gear"
            ,"logo_uantum"
            ,"logo_mechanic"
            ,"logo_gear"
            ,"logo_gear_tail"
            ]
        initial_logo_state = LogoState
            (fromJust $ Map.lookup "logo_the" actor)
            (fromJust $ Map.lookup "logo_uantum" actor)
            (fromJust $ Map.lookup "logo_mechanic" actor)
            (fromJust $ Map.lookup "logo_gear" actor)
            (fromJust $ Map.lookup "logo_gear_tail" actor)
        renderToDocument logo_state =
            svg (document ^. header)
                1
                [defs
                ,renderActor $ logo_state ^. actor_logo_the
                ,renderActor $ logo_state ^. actor_logo_uantum
                ,renderActor $ logo_state ^. actor_logo_mechanic
                ,renderActor $ logo_state ^. actor_logo_gear
                ,renderActor $ logo_state ^. actor_logo_gear_tail
                ]
    viewPresentation Serial initial_logo_state renderToDocument $ do
        actor_logo_the . attributes . x .= -380
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
