{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.SVG where

import Control.Lens ((&),(.~),(%~),(^.),Lens',makeLenses, lens, to)
import Control.Monad (forM_)
import Control.Monad.Trans.State.Strict (execState,get,put)

import Data.Attoparsec.Text (Parser, choice, double, endOfInput, parseOnly, string)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text,pack,unpack)

import Text.XML (Document(..),Element(..),Name(..),Node(..),Prologue(..))

import Slick.Transition (Interpolatable(..))

import Debug.Trace

parseSize :: Element → Text → Double
parseSize Element{..} name =
    case parseOnly size_parser (fromMaybe (error $ unpack name ++" field does not exist.") $ Map.lookup (Name name Nothing Nothing) elementAttributes) of
        Left _ → error $ "Invalid format for the " ++ unpack name ++ " attribute."
        Right result → result

size_parser :: Parser Double
size_parser =
    (*) <$> double
        <*> choice
                [string "pt" >> return 1.25
                ,string "pc" >> return 1.5
                ,string "mm" >> return 3.543307
                ,string "cm" >> return 35.43307
                ,string "in" >> return 90
                ,endOfInput >> return 1
                ]


data Header = Header
    {   _header_width :: Double
    ,   _header_height :: Double
    }
makeLenses ''Header

element_attributes :: Lens' Element (Map Name Text)
element_attributes = lens getter setter
  where
    getter = elementAttributes
    setter element new_attributes = element{elementAttributes=new_attributes}

elementAttribute :: Name → Lens' Element Text
elementAttribute name = lens getter setter
  where
    getter Element{..} = fromMaybe "" $ Map.lookup name elementAttributes
    setter element new_value = element & element_attributes %~ Map.insert name new_value

header :: Lens' Document Header
header = lens extractHeader replaceHeader
  where
    extractHeader :: Document → Header
    extractHeader Document{..} =
        Header
            (parseSize documentRoot "width")
            (parseSize documentRoot "height")

    replaceHeader :: Document → Header → Document
    replaceHeader document (Header width height) =
        document
        &
        root_element_attributes %~
            (Map.insert "width" (pack $ show width)
             .
             Map.insert "height" (pack $ show height)
            )

document_root = lens getter setter
  where
    getter = documentRoot
    setter document new_root = document{documentRoot = new_root}

root_element_attributes = document_root . element_attributes

scaleDocument :: Double → Document → Document
scaleDocument scale document =
    (header .~ Header (width*scale) (height*scale))
    .
    (document_root . elementAttribute "transform" %~
        (\old_transform → old_transform
            <> (pack $ "scale(" ++ show scale ++ ")")
            <> (pack $ "translate(" ++ show (-dx) ++ " " ++ show (-dy) ++ ")")
        )
    )
    $
    document
  where
    Header width height = document ^. header
    dx = width*(scale-1)/2
    dy = height*(scale-1)/2

svg :: Header → [Element] → Document
svg header elements =
    Document
        (Prologue [] Nothing [])
        (Element
            (mkName "svg")
            (Map.fromList
                [("xmlns","http://www.w3.org/2000/svg")
                ,("xmlns:xlink","http://www.w3.org/1999/xlink")
                ,("version","1.1")
                ,("width",pack . show $ (header ^. header_width))
                ,("height",pack . show $ (header ^. header_height))
                ,("viewBox",pack $ "0 0 " ++
                    show (header ^. header_width) ++
                    " " ++
                    show (header ^. header_height)
                 )
                ]
            )
            (map NodeElement elements)
        )
        []

mkName :: Text → Name
mkName name = Name name Nothing Nothing

mkDefs :: [Element] → Element
mkDefs = Element (mkName "defs") Map.empty . map NodeElement

extractElementsFromSVG :: Document → [Element]
extractElementsFromSVG Document{documentRoot=Element{..}} = [element | NodeElement element ← elementNodes]

mkDefsFromSVG :: Document → Element
mkDefsFromSVG = mkDefs . extractElementsFromSVG

extractElementsFromAllSVG :: [Document] → [Element]
extractElementsFromAllSVG = concat . map extractElementsFromSVG

mkDefsFromAllSVG :: [Document] → Element
mkDefsFromAllSVG = mkDefs . extractElementsFromAllSVG

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
    {   useId
    ,   useParentTransform :: Text
    ,   _rotation_angle :: Double
    ,   _rotation_x :: Double
    ,   _rotation_y :: Double
    ,   _scale :: Scale
    ,   _x :: Double
    ,   _y :: Double
    } deriving (Eq,Ord,Read,Show)

makeLenses ''Use

mkUse :: Text → Text → Use
mkUse use_id parent_transform = Use use_id parent_transform 0 0 0 (PropScale 1) 0 0

use :: Use → Element
use Use{..} =
    Element
        (mkName "use")
        (Map.fromList
            [(mkName "transform",transform)
            ,("xlink:href","#" <> useId)
            ]
        )
        []
  where
    transform =
        useParentTransform
        <>
        (pack $
            "rotate(" ++ show _rotation_angle ++ " " ++ show _rotation_x ++ " " ++ show _rotation_y ++ ")" ++
            "scale(" ++ (
                case _scale of
                    PropScale scale → show scale
                    NonPropScale x y → show x ++ " " ++ show y
             ) ++ ")" ++
             "translate(" ++ show _x ++ " " ++ show _y ++ ")"
        )

extractElementsForUse :: Document → Set Text → Map Text Use
extractElementsForUse Document{..} id_set =
    if not (Set.null remaining_id_set)
    then error $ "Some ids were not found: " ++ show (Set.toList remaining_id_set)
    else id_map
  where
    (id_map,remaining_id_set) = flip execState (mempty, id_set) $ goElement mempty documentRoot

    goElement transform Element{..} = do
        (id_map, remaining_id_set) ← get
        case Map.lookup "id" elementAttributes of
            Just id'
              | Set.member id' remaining_id_set →
                    put (Map.insert id' (mkUse id' new_transform) id_map, Set.delete id' remaining_id_set)
            _ → return ()
        forM_ [element | NodeElement element ← elementNodes] $ goElement new_transform
      where
        new_transform =
            case Map.lookup "transform" elementAttributes of
                Nothing → transform
                Just new_transform → transform <> new_transform
