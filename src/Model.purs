module Neuron.Model where

import Prelude

import Data.Array ((!!), length, filter, mapWithIndex)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Lazy (defer, force)
import Data.Lens (Prism', (.~), (%~), prism')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Type.Proxy (Proxy(..))

type Drawing = Array Boolean

type Edge 
  = { from :: Int
    , coeff :: Int
    }

data Neuron = Captor Int | From { coeffs :: Array Edge, threshold :: Int }

_from :: Prism' Neuron { coeffs :: Array Edge, threshold :: Int }
_from = prism' From case _ of
  From e -> Just e
  _ -> Nothing

type Model
  = { drawings :: Array Drawing
    , neurons :: Array Neuron
    , values :: Array Int
    , selectedDrawing :: Int
    , selectedCaptor :: Maybe Int
    , selectedNeuron :: Int
    , editorOpen :: Boolean
    }

drawingA :: Array Boolean
drawingA =
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 0, 0, 1, 1, 0, 0, 0
  , 0, 0, 0, 0, 1, 1, 0, 0, 0
  , 0, 0, 0, 1, 0, 0, 1, 0, 0
  , 0, 0, 1, 1, 1, 1, 1, 0, 0
  , 0, 0, 1, 0, 0, 0, 1, 0, 0
  , 0, 1, 0, 0, 0, 0, 1, 0, 0
  , 0, 0, 0, 0, 0, 0, 0, 0, 0
  ] <#> (_ == 1)

drawingB :: Array Boolean
drawingB =
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 1, 1, 1, 0, 0, 0, 0
  , 0, 0, 1, 0, 1, 0, 0, 0, 0
  , 0, 0, 1, 0, 1, 0, 0, 0, 0
  , 0, 0, 1, 1, 1, 1, 0, 0, 0
  , 0, 0, 1, 0, 0, 1, 0, 0, 0
  , 0, 0, 1, 0, 0, 1, 0, 0, 0
  , 0, 0, 1, 1, 1, 1, 0, 0, 0
  , 0, 0, 0, 0, 0, 0, 0, 0, 0
  ] <#> (_ == 1)

drawingC :: Array Boolean
drawingC =
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 0, 1, 1, 1, 1, 0, 0
  , 0, 0, 1, 0, 0, 0, 0, 0, 0
  , 0, 1, 0, 0, 0, 0, 0, 0, 0
  , 0, 1, 0, 0, 0, 0, 0, 0, 0
  , 0, 1, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 1, 0, 0, 0, 0, 0, 0
  , 0, 0, 0, 1, 1, 1, 0, 0, 0
  , 0, 0, 0, 0, 0, 0, 0, 0, 0
  ] <#> (_ == 1)

drawingD :: Array Boolean
drawingD =
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 1, 1, 1, 0, 0, 0, 0
  , 0, 1, 1, 0, 0, 1, 0, 0, 0
  , 0, 0, 1, 0, 0, 0, 1, 0, 0
  , 0, 0, 1, 0, 0, 0, 1, 0, 0
  , 0, 0, 1, 0, 0, 0, 1, 0, 0
  , 0, 0, 1, 0, 0, 1, 0, 0, 0
  , 0, 1, 1, 1, 1, 0, 0, 0, 0
  , 0, 0, 0, 0, 0, 0, 0, 0, 0
  ] <#> (_ == 1)

init :: Model
init =
  { drawings: [ drawingA, drawingB, drawingC, drawingD ]
  , neurons :
    [ Captor 1, Captor 2, Captor 3, Captor 3, Captor 4, Captor 5 
    
    , From {coeffs: [ {from: 1, coeff: 1}, {from: 4, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 0, coeff: 1}, {from: 2, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 2, coeff: 1}, {from: 5, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 4, coeff: 1}, {from: 5, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 3, coeff: 1}, {from: 4, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 1, coeff: 1}, {from: 5, coeff: 1} ], threshold: 1}
    
    , From {coeffs: [ {from: 6, coeff: 1}, {from: 7, coeff: 1}, {from: 8, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 6, coeff: 1}, {from: 9, coeff: 1}, {from: 10, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 7, coeff: 1}, {from: 9, coeff: 1}, {from: 11, coeff: 1} ], threshold: 1}
    , From {coeffs: [ {from: 8, coeff: 1}, {from: 10, coeff: 1}, {from: 11, coeff: 1} ], threshold: 1}
    ]
  , values: []
  , selectedDrawing: 0
  , selectedCaptor: Nothing
  , selectedNeuron: 6
  , editorOpen: false
  } # simulate

countPixels :: Int -> Drawing -> Int
countPixels i =
  length <<< filter identity <<< mapWithIndex (\j b -> 
    let row = j `div` 27
        col = (j `mod` 9) `div` 3
    in
    b && if i < 3 then row == i else col == i - 3
  )

simulate :: Model -> Model
simulate model@{drawings, selectedDrawing, neurons} = model { values = force <$> values }
  where
    values =
      neurons # mapWithIndex \i neuron -> defer \_ ->
        case neuron of
          Captor _ -> countPixels i drawing
          From {coeffs, threshold} ->
            let
              s = sum $ coeffs <#> \{from, coeff} -> coeff * maybe 0 force (values !! from)
            in
              if s >= threshold then s else 0
    drawing = fromMaybe [] $ drawings !! selectedDrawing
  

data Msg
  = SelectCaptor (Maybe Int)
  | SelectDrawing Int
  | SelectNeuron Int
  | ChangeCoeff Int String
  | ChangeThreshold String
  | OpenDrawingEditor Boolean
  | ChangePixel Int
  | ResetDrawing

update :: Msg -> Model -> Model
update msg model = case msg of
  SelectCaptor i -> model{selectedCaptor = i}
  SelectDrawing i -> simulate $ model{selectedDrawing = i}
  SelectNeuron i -> model{selectedNeuron = i}
  ChangeCoeff i s -> case Int.fromString s of
                      Nothing -> model
                      Just val ->
                        simulate $ model # 
                        (prop (Proxy ∷ _ "neurons") <<< ix model.selectedNeuron <<< _from 
                        <<< prop (Proxy :: _ "coeffs") <<< ix i <<< prop (Proxy :: _ "coeff")
                        ) .~ val
  ChangeThreshold s -> case Int.fromString s of
                      Nothing -> model
                      Just val ->
                        simulate $ model # 
                        (prop (Proxy ∷ _ "neurons") <<< ix model.selectedNeuron <<< _from 
                          <<< prop (Proxy :: _ "threshold")
                        ) .~ val
  OpenDrawingEditor b -> model{editorOpen = b}
  ChangePixel i -> model # (prop (Proxy ∷ _ "drawings") <<< ix model.selectedDrawing <<< ix i) %~ not
  ResetDrawing -> model # (prop (Proxy ∷ _ "drawings") <<< ix model.selectedDrawing) .~
                    case model.selectedDrawing of
                      0 -> drawingA
                      1 -> drawingB
                      2 -> drawingC
                      _ -> drawingD