module Neuron.Model where

import Prelude

import Data.Array ((!!), mapWithIndex)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Lazy (defer, force)
import Data.Lens (Prism', (.~), (%~), prism')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (exp)
import Type.Proxy (Proxy(..))
import Neuron.Util (count)

step :: Number
step = 0.0001 -- le pas dans le flot gradient
coef :: Number
coef = 0.7 -- coef dans la fonction de coût d'erreur qui est une fonction exponentielle

type Pattern = {symbol :: Int, pattern :: Array Boolean, selected :: Boolean}

type Edge 
  = { from :: Int
    , coeff :: Int
    }

data Neuron = Input Int | Neuron { coeffs :: Array Edge, threshold :: Int }

_Neuron :: Prism' Neuron { coeffs :: Array Edge, threshold :: Int }
_Neuron = prism' Neuron case _ of
  Neuron e -> Just e
  _ -> Nothing

type Model
  = { patterns :: Array Pattern
    , neurons :: Array Neuron
    , values :: Array Int
    , currentPattern :: Int
    , selectedInput :: Maybe Int
    , selectedNeuron :: Int
    , editorOpen :: Boolean
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
  { patterns:
    [ {symbol: 0, pattern: patternA, selected: true}
    , {symbol: 0, pattern: patternA, selected: false}
    , {symbol: 0, pattern: patternA, selected: false}
    , {symbol: 0, pattern: patternA, selected: false}
    , {symbol: 0, pattern: patternA, selected: false}
    , {symbol: 0, pattern: patternA, selected: false}

    , {symbol: 1, pattern: patternB, selected: true}
    , {symbol: 1, pattern: patternB, selected: false}
    , {symbol: 1, pattern: patternB, selected: false}
    , {symbol: 1, pattern: patternB, selected: false}
    , {symbol: 1, pattern: patternB, selected: false}
    , {symbol: 1, pattern: patternB, selected: false}

    , {symbol: 2, pattern: patternC, selected: true}
    , {symbol: 2, pattern: patternC, selected: false}
    , {symbol: 2, pattern: patternC, selected: false}
    , {symbol: 2, pattern: patternC, selected: false}
    , {symbol: 2, pattern: patternC, selected: false}
    , {symbol: 2, pattern: patternC, selected: false}

    , {symbol: 3, pattern: patternD, selected: true}
    , {symbol: 3, pattern: patternD, selected: false}
    , {symbol: 3, pattern: patternD, selected: false}
    , {symbol: 3, pattern: patternD, selected: false}
    , {symbol: 3, pattern: patternD, selected: false}
    , {symbol: 3, pattern: patternD, selected: false}
    ]
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
  , currentPattern: 0
  , selectedInput: Nothing
  , selectedNeuron: 6
  , editorOpen: false
  } # simulate

indexPattern :: Array Pattern -> Int -> Pattern
indexPattern ps i = fromMaybe {selected: false, symbol: 0, pattern: []} $ ps !! i

countPixels :: Int -> Array Boolean -> Int
countPixels i =
  count identity <<< mapWithIndex (\j b -> 
    let row = j `div` 27
        col = (j `mod` 9) `div` 3
    in
    b && if i < 3 then row == i else col == i - 3
  )

cost :: Number -> Number -> Boolean -> Number
cost x a true = -a * exp (-a * x)
cost x a false = a * exp (a * x)

simulate :: Model -> Model
simulate model@{patterns, currentPattern, neurons} = model { values = force <$> values }
    where
      {pattern} = indexPattern patterns currentPattern
      values = neurons <#> \neuron -> defer \_ ->
          case neuron of
            Input i -> countPixels i pattern
            Neuron {coeffs, threshold} ->
              let
                s = sum $ coeffs <#> \{from, coeff} -> coeff * maybe 0 force (values !! from)
              in
                max 0 (s - threshold)

data Msg
  = SelectInput (Maybe Int)
  | SelectPattern Int
  | SelectNeuron Int
  | ChangeCoeff Int String
  | ChangeThreshold String
  | OpenPatternEditor Boolean
  | ChangePixel Int
  | ResetPattern

update :: Msg -> Model -> Model
update msg model = case msg of
  SelectInput i -> model{selectedInput = i}
  SelectPattern i -> simulate $ model{currentPattern = i}
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
  ChangePixel i -> simulate $ model # (prop (Proxy ∷ _ "patterns") 
                                        <<< ix model.currentPattern 
                                        <<< prop (Proxy :: _ "pattern")
                                        <<< ix i) %~ not
  ResetPattern -> simulate $ model # (prop (Proxy ∷ _ "patterns") <<< ix model.currentPattern <<< prop (Proxy :: _ "pattern")) .~
                    case model.currentPattern of
                      0 -> patternA
                      1 -> patternB
                      2 -> patternC
                      _ -> patternD