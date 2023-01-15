module Neuron.Model where

import Prelude

import Data.Array ((!!), mapWithIndex, replicate)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Lazy (defer, force)
import Data.Lens (Lens', Prism', (.~), (%~), prism')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (exp)
import Data.Number as Number
import Neuron.Patterns (pattern01, pattern02, pattern03, pattern04, pattern31, pattern32, pattern33, pattern34, pattern61, pattern62, pattern63, pattern64, pattern91, pattern92, pattern93, pattern94, emptyPattern)
import Neuron.Util (count, (!!!))
import Type.Proxy (Proxy(..))

step :: Number
step = 0.0001 -- le pas dans le flot gradient
coef :: Number
coef = 0.7 -- coef dans la fonction de coût d'erreur qui est une fonction exponentielle

type Pattern = {symbol :: Int, pattern :: Array Boolean, selected :: Boolean}

type Edge 
  = { from :: Int
    , coeff :: Number
    }

data Neuron = Input Int | Neuron { coeffs :: Array Edge, threshold :: Number }

data Dialog = EditorDialog | NeuronDialog Int | NoDialog

type Value = {value :: Number, error :: Number}

type State = { neurons :: Array Neuron, values :: Array (Array Value) }

type Model
  = { patterns :: Array Pattern
    , states :: Array State
    , currentPattern :: Int
    , currentState :: Int
    , selectedInput :: Maybe Int
    , dialog :: Dialog
    , editMode :: Boolean
    }

_patterns :: Lens' Model (Array Pattern)
_patterns = prop (Proxy :: _ "patterns")

_states :: Lens' Model (Array State)
_states = prop (Proxy :: _ "states")

_neurons :: Lens' State (Array Neuron)
_neurons = prop (Proxy :: _ "neurons")

_Neuron :: Prism' Neuron { coeffs :: Array Edge, threshold :: Number }
_Neuron = prism' Neuron case _ of
  Neuron e -> Just e
  _ -> Nothing

_coeffs :: Lens' { coeffs :: Array Edge, threshold :: Number } (Array Edge)
_coeffs = prop (Proxy :: _ "coeffs")

_threshold :: Lens' { coeffs :: Array Edge, threshold :: Number } Number
_threshold = prop (Proxy :: _ "threshold")


initPatterns :: Array Pattern
initPatterns =
  [ {symbol: 0, pattern: pattern01, selected: true}
  , {symbol: 0, pattern: pattern02, selected: true}
  , {symbol: 0, pattern: pattern03, selected: true}
  , {symbol: 0, pattern: pattern04, selected: true}
  , {symbol: 0, pattern: emptyPattern, selected: false}
  , {symbol: 0, pattern: emptyPattern, selected: false}

  , {symbol: 1, pattern: pattern31, selected: true}
  , {symbol: 1, pattern: pattern32, selected: true}
  , {symbol: 1, pattern: pattern33, selected: true}
  , {symbol: 1, pattern: pattern34, selected: true}
  , {symbol: 1, pattern: emptyPattern, selected: false}
  , {symbol: 1, pattern: emptyPattern, selected: false}

  , {symbol: 2, pattern: pattern61, selected: true}
  , {symbol: 2, pattern: pattern62, selected: true}
  , {symbol: 2, pattern: pattern63, selected: true}
  , {symbol: 2, pattern: pattern64, selected: true}
  , {symbol: 2, pattern: emptyPattern, selected: false}
  , {symbol: 2, pattern: emptyPattern, selected: false}

  , {symbol: 3, pattern: pattern91, selected: true}
  , {symbol: 3, pattern: pattern92, selected: true}
  , {symbol: 3, pattern: pattern93, selected: true}
  , {symbol: 3, pattern: pattern94, selected: true}
  , {symbol: 3, pattern: emptyPattern, selected: false}
  , {symbol: 3, pattern: emptyPattern, selected: false}
  ]


initNeurons :: Array Neuron
initNeurons =
  [ Input 0, Input 1, Input 2, Input 3, Input 4, Input 5

  , Neuron {coeffs: [ {from: 0, coeff: -1.0}, {from: 1, coeff: 1.0}, {from: 2, coeff: 1.0} ], threshold: 8.0}
  , Neuron {coeffs: [ {from: 0, coeff: 1.0}, {from: 1, coeff: 1.0}, {from: 5, coeff: -1.0} ], threshold: 7.0}
  , Neuron {coeffs: [ {from: 2, coeff: 1.0}, {from: 3, coeff: 1.0}, {from: 4, coeff: 1.0} ], threshold: 18.0}
  , Neuron {coeffs: [ {from: 1, coeff: 1.0}, {from: 3, coeff: -1.0}, {from: 5, coeff: 1.0} ], threshold: 6.0}
  , Neuron {coeffs: [ {from: 0, coeff: 1.0}, {from: 4, coeff: 1.0}, {from: 5, coeff: 1.0} ], threshold: 16.0}
  , Neuron {coeffs: [ {from: 3, coeff: 1.0}, {from: 4, coeff: -1.0}, {from: 5, coeff: 1.0} ], threshold: 5.0}
    
  , Neuron {coeffs: [ {from: 6, coeff: -1.0}
                    , {from: 7, coeff: -1.0}
                    , {from: 8, coeff: 1.0}
                    , {from: 9, coeff: -1.0}
                    , {from: 10, coeff: 1.0}
                    , {from: 11, coeff: 1.0}
                    ], threshold: 7.0}
  , Neuron {coeffs: [ {from: 6, coeff: 1.0}
                    , {from: 7, coeff: 1.0}
                    , {from: 8, coeff: -1.0}
                    , {from: 9, coeff: 1.0}
                    , {from: 10, coeff: -1.0}
                    , {from: 11, coeff: 0.0}
                    ], threshold: 5.0}
  , Neuron {coeffs: [ {from: 6, coeff: -1.0}
                    , {from: 7, coeff: 1.0}
                    , {from: 8, coeff: -1.0}
                    , {from: 9, coeff: 1.0}
                    , {from: 10, coeff: 1.0}
                    , {from: 11, coeff: -1.0}
                    ], threshold: 6.0}
  , Neuron {coeffs: [ {from: 6, coeff: 1.0}
                    , {from: 7, coeff: 1.0}
                    , {from: 8, coeff: 1.0}
                    , {from: 9, coeff: -1.0}
                    , {from: 10, coeff: -1.0}
                    , {from: 11, coeff: 0.0}
                    ], threshold: 5.0}
  ]

init :: Model
init = 
  { patterns: initPatterns
  , states: [{neurons: initNeurons, values: []}]
  , currentState: 0
  , currentPattern: 0
  , selectedInput: Nothing
  , dialog: NoDialog
  , editMode: false
  } # simulate

countPixels :: Int -> Array Boolean -> Int
countPixels i =
  count identity <<< mapWithIndex (\j b -> 
    let row = j `div` 18
        col = (j `mod` 6) `div` 2
    in
    b && if i < 3 then col == i else row == i - 3
  )

cost :: Number -> Number -> Boolean -> Number
cost x a true = -a * exp (-a * x)
cost x a false = a * exp (a * x)

computeValues :: Array Pattern -> Array Neuron -> Array (Array Value)
computeValues patterns neurons =
  patterns <#> \{pattern, selected} -> 
    if selected then
      let
        vals = neurons <#> \neuron -> defer \_ ->
          case neuron of
            Input i -> {value: Int.toNumber $ countPixels i pattern, error: 0.0}
            Neuron {coeffs, threshold} ->
              let
                s = sum $ coeffs <#> \{from, coeff} -> coeff * maybe 0.0 (_.value <<< force) (vals !! from)
              in
                {value: max 0.0 (s - threshold), error: 0.0}
      in force <$> vals
    else
      []

simulate :: Model -> Model
simulate model@{states, patterns} =
  model { states = states <#> \{neurons} ->
    { neurons
    , values: computeValues patterns neurons
    }
  }

{-
runStep :: Array Pattern -> State -> State
runStep patterns {neurons, values} = {neurons: neurons', values: values'} where
  neurons' = neurons # mapWithIndex \i -> case _ of
    Input j -> Input j
    Neuron {coeffs, threshold} ->

  values' = computeValues patterns neurons'
-}

data Msg
  = SelectInput (Maybe Int)
  | SelectPattern Int
  | ChangeCoeff Int Int String
  | ChangeThreshold Int String
  | OpenDialog Dialog
  | ChangePixel Int
  | ResetPattern
  | ToggleEditMode

update :: Msg -> Model -> Model
update msg model = case msg of
  SelectInput i -> model{selectedInput = i}
  SelectPattern i -> simulate $ model{currentPattern = i}
  ChangeCoeff i j s -> case Number.fromString s of
                      Nothing -> model
                      Just val ->
                        simulate $ model # 
                        (_states <<< ix 0 <<< _neurons <<< ix i <<< _Neuron
                        <<< _coeffs <<< ix j <<< prop (Proxy :: _ "coeff")
                        ) .~ val
  ChangeThreshold i s -> case Number.fromString s of
                      Nothing -> model
                      Just val ->
                        simulate $ model # 
                        (_states <<< ix 0 <<< _neurons <<< ix i <<< _Neuron <<< _threshold
                        ) .~ val
  OpenDialog d -> model{dialog = d}
  ChangePixel i -> simulate $ model # (_patterns
                                        <<< ix model.currentPattern 
                                        <<< prop (Proxy :: _ "pattern")
                                        <<< ix i) %~ not
  ToggleEditMode -> model{editMode = not model.editMode}
  ResetPattern -> simulate $ model # (_patterns <<< ix model.currentPattern) .~
                    (initPatterns !!! model.currentPattern)