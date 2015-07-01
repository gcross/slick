{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.SVG where

import qualified Data.Map as Map
import Data.Text (Text,pack)

import Text.Printf (printf)
import Text.XML (Document(..),Element(..),Name(..),Node(..),Prologue(..))

import Slick.Transition (Interpolatable(..))

data Header = Header
    {   headerWidth :: Double
    ,   headerHeight :: Double
    }

svg :: Header → Element → Document
svg Header{..} element =
    Document
        (Prologue [] Nothing [])
        (Element
            (mkName "svg")
            (Map.fromList
                [("xmlns",svgns)
                ,("width",pack . show $ headerWidth)
                ,("height",pack . show $ headerHeight)
                ]
            )
            [NodeElement element]
        )
        []

svgns :: Text
svgns = "http://www.w3.org/2000/svg"

-- svgName :: Text → Name
-- svgName name = Name name (Just svgns) Nothing

mkName :: Text → Name
mkName name = Name name Nothing Nothing

defs :: [Element] → Element
defs = Element (mkName "defs") Map.empty . map NodeElement

extractElementsFromSVG :: Document → [Element]
extractElementsFromSVG Document{documentRoot=Element{..}} = [element | NodeElement element ← elementNodes]

extractElementsFromAllSVG :: [Document] → [Element]
extractElementsFromAllSVG = concat . map extractElementsFromSVG

data Scale =
    PropScale Double
  | NonPropScale Double Double
  deriving (Eq,Ord,Read,Show)

instance Interpolatable Double Scale where
    interpolateUnitInterval (PropScale before) (PropScale after) t =
        PropScale (interpolateUnitInterval before after t)
    interpolateUnitInterval (NonPropScale before1 before2) (NonPropScale after1 after2) t =
        NonPropScale (interpolateUnitInterval before1 after1 t) (interpolateUnitInterval before2 after2 t)
    interpolateUnitInterval (PropScale _) (NonPropScale _ _) _ =
        error "Must interpolate between the same kind of scale.  (Not from PropScale to NonPropScale.)"
    interpolateUnitInterval (NonPropScale _ _) (PropScale _) _ =
        error "Must interpolate between the same kind of scale.  (Not from NonPropScale to PropScale.)"

data Use = Use
    {   useId :: Text
    ,   useRotation :: Double
    ,   useScale :: Scale
    ,   useX :: Double
    ,   useY :: Double
    } deriving (Eq,Ord,Read,Show)

newUse :: Text → Use
newUse use_id = Use use_id 0 (PropScale 1) 0 0

use :: Use → Element
use Use{..} =
    Element
        (mkName "use")
        (Map.singleton "transform"
         .
         pack
         $
         printf "rotate(%d)scale(%s)translate(%d %d)"
            useRotation
            (case useScale of
                PropScale scale → show scale
                NonPropScale x y → show x ++ " " ++ show y
            )
            useX
            useY
        )
        []
