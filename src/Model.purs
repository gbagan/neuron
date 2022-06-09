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

type Pattern = Array Boolean

type Edge 
  = { from :: Int
    , coeff :: Int
    }

data Neuron = Input Int | Neuron { coeffs :: Array Edge, threshold :: Int }

_Neuron :: Prism' Neuron { coeffs :: Array Edge, threshold :: Int }
_Neuron = prism' Neuron case _ of
  Neuron e -> Just e
  _ -> Nothing

data ThresholdRule = StandardRule | BooleanRule

derive instance Eq ThresholdRule

thresholdRuleToString :: ThresholdRule -> String
thresholdRuleToString StandardRule = "standard"
thresholdRuleToString BooleanRule = "bool"

type Model
  = { patterns :: Array Pattern
    , neurons :: Array Neuron
    , values :: Array Int
    , selectedPattern :: Int
    , selectedInput :: Maybe Int
    , selectedNeuron :: Int
    , editorOpen :: Boolean
    , thresholdRule :: ThresholdRule
    }

patternA :: Array Boolean
patternA =
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

patternB :: Array Boolean
patternB =
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

patternC :: Array Boolean
patternC =
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

patternD :: Array Boolean
patternD =
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
  { patterns: [ patternA, patternB, patternC, patternD ]
  , neurons :
    [ Input 0, Input 1, Input 2, Input 3, Input 4, Input 5 
    
    , Neuron {coeffs: [ {from: 1, coeff: 1}, {from: 4, coeff: 1} ], threshold: 1}
    , Neuron {coeffs: [ {from: 0, coeff: 1}, {from: 2, coeff: 1} ], threshold: 1}
    , Neuron {coeffs: [ {from: 2, coeff: 1}, {from: 5, coeff: 1} ], threshold: 1}
    , Neuron {coeffs: [ {from: 4, coeff: 1}, {from: 5, coeff: 1} ], threshold: 1}
    , Neuron {coeffs: [ {from: 3, coeff: 1}, {from: 4, coeff: 1} ], threshold: 1}
    , Neuron {coeffs: [ {from: 1, coeff: 1}, {from: 5, coeff: 1} ], threshold: 1}
    
    , Neuron {coeffs: [ {from: 6, coeff: 1}, {from: 7, coeff: 1}, {from: 8, coeff: 1} ], threshold: 40}
    , Neuron {coeffs: [ {from: 6, coeff: 1}, {from: 9, coeff: 1}, {from: 10, coeff: 1} ], threshold: 1}
    , Neuron {coeffs: [ {from: 7, coeff: 1}, {from: 9, coeff: 1}, {from: 11, coeff: 1} ], threshold: 1}
    , Neuron {coeffs: [ {from: 8, coeff: 1}, {from: 10, coeff: 1}, {from: 11, coeff: 1} ], threshold: 1}
    ]
  , values: []
  , selectedPattern: 0
  , selectedInput: Nothing
  , selectedNeuron: 6
  , editorOpen: false
  , thresholdRule: StandardRule
  } # simulate

countPixels :: Int -> Pattern -> Int
countPixels i =
  length <<< filter identity <<< mapWithIndex (\j b -> 
    let row = j `div` 27
        col = (j `mod` 9) `div` 3
    in
    b && if i < 3 then row == i else col == i - 3
  )

simulate :: Model -> Model
simulate model@{patterns, selectedPattern, neurons, thresholdRule} = model { values = force <$> values }
  where
    values =
      neurons # mapWithIndex \i neuron -> defer \_ ->
        case neuron of
          Input _ -> countPixels i pattern
          Neuron {coeffs, threshold} ->
            let
              s = sum $ coeffs <#> \{from, coeff} -> coeff * maybe 0 force (values !! from)
            in
              if s < threshold then
                0
              else if thresholdRule == StandardRule then
                s
              else
                1
    pattern = fromMaybe [] $ patterns !! selectedPattern
  

data Msg
  = SelectInput (Maybe Int)
  | SelectPattern Int
  | SelectNeuron Int
  | ChangeCoeff Int String
  | ChangeThreshold String
  | OpenPatternEditor Boolean
  | ChangePixel Int
  | ResetPattern
  | SetThresholdRule String

update :: Msg -> Model -> Model
update msg model = case msg of
  SelectInput i -> model{selectedInput = i}
  SelectPattern i -> simulate $ model{selectedPattern = i}
  SelectNeuron i -> model{selectedNeuron = i}
  ChangeCoeff i s -> case Int.fromString s of
                      Nothing -> model
                      Just val ->
                        simulate $ model # 
                        (prop (Proxy ∷ _ "neurons") <<< ix model.selectedNeuron <<< _Neuron
                        <<< prop (Proxy :: _ "coeffs") <<< ix i <<< prop (Proxy :: _ "coeff")
                        ) .~ val
  ChangeThreshold s -> case Int.fromString s of
                      Nothing -> model
                      Just val ->
                        simulate $ model # 
                        (prop (Proxy ∷ _ "neurons") <<< ix model.selectedNeuron <<< _Neuron
                          <<< prop (Proxy :: _ "threshold")
                        ) .~ val
  OpenPatternEditor b -> model{editorOpen = b}
  ChangePixel i -> simulate $ model # (prop (Proxy ∷ _ "patterns") <<< ix model.selectedPattern <<< ix i) %~ not
  ResetPattern -> simulate $ model # (prop (Proxy ∷ _ "patterns") <<< ix model.selectedPattern) .~
                    case model.selectedPattern of
                      0 -> patternA
                      1 -> patternB
                      2 -> patternC
                      _ -> patternD
  SetThresholdRule s -> case s of
    "standard" -> simulate $ model{thresholdRule = StandardRule}
    "bool" -> simulate $ model{thresholdRule = BooleanRule}
    _ -> model