{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import System.Environment

import qualified Text.XML as XML

import Slick.Render
import Slick.SVG

main = do
    -- filename:_ ← getArgs
    let filename = "quantum_mechanic.svg"
    document ← XML.readFile def filename
    let header = extractHeader document
        defs = mkDefsFromSVG document
        uses = extractElementsForUse document (Set.fromList ["logo_gear","logo_the","logo_uantum","logo_gear_tail","logo_Mechanic"])

        new_document =
            svg header
                [defs
                ,(use (fromJust $ Map.lookup "logo_gear" uses))
                ,(use (fromJust $ Map.lookup "logo_the" uses))
                ,(use (fromJust $ Map.lookup "logo_uantum" uses))
                ,(use (fromJust $ Map.lookup "logo_gear_tail" uses))
                ,(use (fromJust $ Map.lookup "logo_Mechanic" uses))
                ]
    XML.writeFile def "new_document.svg" new_document
    viewDocument new_document
