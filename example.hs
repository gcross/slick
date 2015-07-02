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
    filename:_ ← getArgs
    document ← XML.readFile def filename
    let header = extractHeader document
        defs = mkDefsFromSVG document
        uses = extractElementsForUse document (Set.fromList ["logo_gear"])

        new_document =
            svg header
                [defs
                ,(use (fromJust $ Map.lookup "logo_gear" uses))
                ]
    XML.writeFile def "new_document.svg" new_document
    viewDocument new_document
