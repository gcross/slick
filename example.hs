{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Default (def)

import System.Environment

import qualified Text.XML as XML

import Slick.Render

main = do
    filename:_ ← getArgs
    XML.readFile def filename >>= viewDocument
