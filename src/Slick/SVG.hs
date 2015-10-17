{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Slick.SVG where

import Control.Lens ((&),(.~),(%~),(^.),Lens',makeLenses, lens)
import Control.Monad (forM_)
import Control.Monad.Trans.State.Strict (execState,get,put)

import Data.Attoparsec.Text (Parser, choice, double, endOfInput, parseOnly, string)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text,pack,unpack)

import Text.XML (Document(..),Element(..),Name(..),Node(..),Prologue(..))

import Slick.Transition (Interpolatable(..))

data Header = Header
    {   _header_width :: Double
    ,   _header_height :: Double
    }
makeLenses ''Header

data Scale =
    PropScale Double
  | NonPropScale Double Double
  deriving (Eq,Ord,Read,Show)

instance Default Scale where
    def = PropScale 1

data Attributes = Attributes
    {   _rotation_angle :: Double
    ,   _rotation_x :: Double
    ,   _rotation_y :: Double
    ,   _scale :: Scale
    ,   _x :: Double
    ,   _y :: Double
    ,   _opacity :: Double
    } deriving (Eq,Ord,Read,Show)
makeLenses ''Attributes

instance Default Attributes where
    def = Attributes 0 0 0 def 0 0 1

data Actor = Actor
    {   actorId
    ,   actorParentTransform :: Text
    ,   _attributes :: Attributes
    } deriving (Eq,Ord,Read,Show)
makeLenses ''Actor

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

transformScale scale = pack $ "scale(" ++ show scale ++ ")"
transformScaleBoth scale_x scale_y = pack $ "scale(" ++ show scale_x ++ " " ++ show scale_y ++ ")"
transformTranslate dx dy = pack $ "translate(" ++ show dx ++ " " ++ show dy ++ ")"

groupTransformAndStyle :: Text → Text → [Element] → Element
groupTransformAndStyle transform style elements =
    Element
        (mkName "g")
        (Map.fromList
            [(mkName "transform", transform)
            ,(mkName "style", style)
            ]
        )
        (map NodeElement elements)

svg :: Header → Double → [Element] → Document
svg header scale elements =
    Document
        (Prologue [] Nothing [])
        (Element
            (mkName "svg")
            (Map.fromList
                [("xmlns","http://www.w3.org/2000/svg")
                ,("xmlns:xlink","http://www.w3.org/1999/xlink")
                ,("version","1.1")
                ,("width",pack . show $ width)
                ,("height",pack . show $ height)
                ,("viewBox",pack $ "0 0 " ++ show width ++ " " ++ show height)
                ,("transform",transform)
                ]
            )
            (map NodeElement elements)
        )
        []
  where
    width = header ^. header_width * scale
    height = header ^. header_height * scale
    initial_x = header ^. header_width / 2
    initial_y = header ^. header_height / 2
    final_x = initial_x*scale
    final_y = initial_y*scale
    transform =
        transformTranslate (-final_x) (-final_y)
        <>
        transformScale scale
        <>
        transformTranslate initial_x initial_y

mkName :: Text → Name
mkName name = Name name Nothing Nothing

mkDefs :: [Element] → Element
mkDefs = Element (mkName "defs") Map.empty . map NodeElement

extractElementsFromSVG :: Document → [Element]
extractElementsFromSVG Document{documentRoot=Element{..}} = [element | NodeElement element ← elementNodes]

mkDefsFromSVG :: Document → Element
mkDefsFromSVG = mkDefs . extractElementsFromSVG

extractElementsFromAllSVG :: [Document] → [Element]
extractElementsFromAllSVG = concatMap extractElementsFromSVG

mkDefsFromAllSVG :: [Document] → Element
mkDefsFromAllSVG = mkDefs . extractElementsFromAllSVG

instance Interpolatable Scale where
    interpolateUnitInterval (PropScale before) (PropScale after) t =
        PropScale (interpolateUnitInterval before after t)
    interpolateUnitInterval (NonPropScale before1 before2) (NonPropScale after1 after2) t =
        NonPropScale (interpolateUnitInterval before1 after1 t) (interpolateUnitInterval before2 after2 t)
    interpolateUnitInterval (PropScale _) (NonPropScale _ _) _ =
        error "Must interpolate between the same kind of scale.  (Not from PropScale to NonPropScale.)"
    interpolateUnitInterval (NonPropScale _ _) (PropScale _) _ =
        error "Must interpolate between the same kind of scale.  (Not from NonPropScale to PropScale.)"

mkActor :: Text → Text → Actor
mkActor actor_id parent_transform = Actor actor_id parent_transform def

renderAttributesTransform :: Attributes → Text
renderAttributesTransform Attributes{..} = pack $
    "scale(" ++ (
        case _scale of
            PropScale scale → show scale
            NonPropScale x y → show x ++ " " ++ show y
     ) ++ ")" ++
     "translate(" ++ show _x ++ " " ++ show _y ++ ")" ++
     "rotate(" ++ show _rotation_angle ++ " " ++ show _rotation_x ++ " " ++ show _rotation_y ++ ")"


renderActor :: Actor → Element
renderActor actor =
    Element
        (mkName "use")
        (Map.fromList
            [(mkName "transform",transform)
            ,(mkName "opacity",pack . show $ actor ^. attributes . opacity)
            ,("xlink:href","#" <> actorId actor)
            ]
        )
        []
  where
    transform =
        actorParentTransform actor
        <>
        renderAttributesTransform (actor ^. attributes)

extractActors :: Document → Set Text → Map Text Actor
extractActors Document{..} id_set =
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
                    put (Map.insert id' (mkActor id' new_transform) id_map, Set.delete id' remaining_id_set)
            _ → return ()
        forM_ [element | NodeElement element ← elementNodes] $ goElement new_transform
      where
        new_transform =
            case Map.lookup "transform" elementAttributes of
                Nothing → transform
                Just new_transform → transform <> new_transform
