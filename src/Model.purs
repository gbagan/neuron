module Neuron.Model where

import Prelude

import Data.Array ((..), elem, filter, mapWithIndex, snoc, sortWith, zipWith)
import Data.Foldable (foldl, sum, minimum, maximum)
import Data.Int (floor, ceil)
import Data.Int as Int
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Neuron.Patterns ( pattern01, pattern02, pattern03, pattern04
                       , pattern31, pattern32, pattern33, pattern34
                       , pattern61, pattern62, pattern63, pattern64
                       , pattern91, pattern92, pattern93, pattern94
                       , emptyPattern)
import Neuron.Util (count, (!), map2)
import Type.Proxy (Proxy(..))

type Pattern = { symbol :: Int, pattern :: Array Boolean, selected :: Boolean }

data Dialog = EditorDialog | NeuronDialog Int Int | NoDialog | AllNeuronsDialog

type State =
  { hiddenThresholds :: Array Number
  , hiddenWeights :: Array (Array Number)
  , finalThresholds :: Array Number
  , finalWeights :: Array (Array Number)
  , output :: Array { hidden :: Array Number, final :: Array Number }
  , iter :: Int
  }

type Model =
  { patterns :: Array Pattern
  , inputs :: Array (Array Number)
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

_hiddenWeights :: Lens' State (Array (Array Number))
_hiddenWeights = prop (Proxy :: _ "hiddenWeights")

_hiddenThresholds :: Lens' State (Array Number)
_hiddenThresholds = prop (Proxy :: _ "hiddenThresholds")

_finalWeights :: Lens' State (Array (Array Number))
_finalWeights = prop (Proxy :: _ "hiddenWeights")

_finalThresholds :: Lens' State (Array Number)
_finalThresholds = prop (Proxy :: _ "hiddenThresholds")

_currentState :: Lens' Model Int
_currentState = prop (Proxy :: _ "currentState")

_selected :: Lens' Pattern Boolean
_selected = prop (Proxy :: _ "selected")

-- | mask ! i ! j <=> there is a link between the the hidden neuron i and the input neuron j
mask :: Array (Array Boolean)
mask =
  [ [ 1, 1, 1, 0, 0, 0 ]
  , [ 1, 1, 0, 0, 0, 1 ]
  , [ 0, 0, 1, 1, 1, 0 ]
  , [ 0, 1, 0, 1, 0, 1 ]
  , [ 1, 0, 0, 0, 1, 1 ]
  , [ 0, 0, 0, 1, 1, 1 ]
  ] <#> map (_ == 1)

initPatterns :: Array Pattern
initPatterns =
  [ { symbol: 0, pattern: pattern01, selected: true }
  , { symbol: 0, pattern: pattern02, selected: true }
  , { symbol: 0, pattern: pattern03, selected: true }
  , { symbol: 0, pattern: pattern04, selected: true }
  , { symbol: 0, pattern: emptyPattern, selected: false }
  , { symbol: 0, pattern: emptyPattern, selected: false }

  , { symbol: 1, pattern: pattern31, selected: true }
  , { symbol: 1, pattern: pattern32, selected: true }
  , { symbol: 1, pattern: pattern33, selected: true }
  , { symbol: 1, pattern: pattern34, selected: true }
  , { symbol: 1, pattern: emptyPattern, selected: false }
  , { symbol: 1, pattern: emptyPattern, selected: false }

  , { symbol: 2, pattern: pattern61, selected: true }
  , { symbol: 2, pattern: pattern62, selected: true }
  , { symbol: 2, pattern: pattern63, selected: true }
  , { symbol: 2, pattern: pattern64, selected: true }
  , { symbol: 2, pattern: emptyPattern, selected: false }
  , { symbol: 2, pattern: emptyPattern, selected: false }

  , { symbol: 3, pattern: pattern91, selected: true }
  , { symbol: 3, pattern: pattern92, selected: true }
  , { symbol: 3, pattern: pattern93, selected: true }
  , { symbol: 3, pattern: pattern94, selected: true }
  , { symbol: 3, pattern: emptyPattern, selected: false }
  , { symbol: 3, pattern: emptyPattern, selected: false }
  ]

initState :: State
initState =
  { hiddenThresholds: [ 8.0, 7.0, 18.0, 6.0, 16.0, 5.0 ]
  , hiddenWeights:
      [ [ -1.0, 1.0, 1.0, 0.0, 0.0, 0.0 ]
      , [ 1.0, 1.0, 0.0, 0.0, 0.0, -1.0 ]
      , [ 0.0, 0.0, 1.0, 1.0, 1.0, 0.0 ]
      , [ 0.0, 1.0, 0.0, -1.0, 0.0, 1.0 ]
      , [ 1.0, 0.0, 0.0, 0.0, 1.0, 1.0 ]
      , [ 0.0, 0.0, 0.0, 1.0, -1.0, 1.0 ]
      ]
  , finalThresholds: [ 7.0, 5.0, 6.0, 5.0 ]
  , finalWeights:
      [ [ -1.0, -1.0, 1.0, -1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, -1.0, 1.0, -1.0, 0.0 ]
      , [ -1.0, 1.0, -1.0, 1.0, 1.0, -1.0 ]
      , [ 1.0, 1.0, 1.0, -1.0, -1.0, 0.0 ]
      ]
  , output: []
  , iter: 0
  }

init :: Model
init =
  { patterns: initPatterns
  , inputs: []
  , states: [ initState ]
  , currentState: 0
  , currentPattern: 0
  , selectedInput: Nothing
  , dialog: NoDialog
  , editMode: false
  } # simulate

countPixels :: Int -> Array Boolean -> Int
countPixels i =
  count identity <<< mapWithIndex
    ( \j b ->
        let
          row = j `div` 18
          col = (j `mod` 6) `div` 2
        in
          b && if i < 3 then col == i else row == i - 3
    )

updateInput :: Array Pattern -> Array (Array Number)
updateInput =
  map \{ pattern } ->
    0..5 <#> \i -> Int.toNumber $ countPixels i pattern

updateOutput :: Array (Array Number) -> State -> State
updateOutput inputs st = st { output = output }
  where
  output = inputs <#> \input ->
    let
      hidden = map2 st.hiddenThresholds st.hiddenWeights \t hw ->
        max 0.0 (sum (zipWith (*) input hw) - t)
      final = map2 st.finalThresholds st.finalWeights \t fw ->
        sum (zipWith (*) hidden fw) - t
    in
      { final, hidden }

simulate :: Model -> Model
simulate model@{ states, patterns } = model { inputs = inputs, states = states <#> updateOutput inputs }
  where
  inputs = updateInput patterns

foreign import runStepImpl :: Array (Array Boolean) -> Model -> State -> State

runStep :: Model -> State -> State
runStep m@{ inputs } st = updateOutput inputs $ runStepImpl mask m st

iterList :: Array Int
iterList = [0, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 40000, 50000, 60000]

runLearning :: Model -> Model
runLearning m@{ states } = m { states = go 0 [] (states ! 0) }
  where
  go n acc st | n==60000 = acc `snoc` st
              | n `elem` iterList = go (n + 1) (acc `snoc` st) (runStep m st)
              | otherwise = go (n+1) acc (runStep m st)


type RulerPositions = 
  { zero :: Number
  , symbols :: Array { symbol :: Int, x :: Number, y :: Number }
  , graduation :: Array { value :: Int, x :: Number }
  }

rulerPositions :: Array Pattern -> State -> Int -> Int -> RulerPositions                    
rulerPositions patterns st i j =
  let
    values = st.output <#> \{hidden, final} -> if i == 1 then hidden ! j else final ! j
    minX = fromMaybe 0.0 $ minimum values
    maxX = fromMaybe 0.0 $ maximum values
    values' = values <#> \v -> (v - minX) / (maxX - minX)
    t = map2 patterns values' (\{selected, symbol} value -> {selected, symbol, value})
          # filter _.selected
          # map (\{symbol, value} -> {symbol, value})
          # sortWith _.value
    go {prev, height, acc} {symbol, value}
      | value - prev >= 0.05 = {prev: value, height: 1.0, acc: acc `snoc` {symbol, x: value, y: 0.0}}
      | otherwise =  {prev, height: height+1.0, acc: acc `snoc` {symbol, x: value, y: height}}
  in
    { zero: - minX / (maxX - minX)
    , symbols: (foldl go {prev: -0.1, height: 0.0, acc: []} t).acc
    , graduation: floor minX .. ceil maxX <#> \k -> {value: k, x: (Int.toNumber k - minX) / (maxX - minX)}
    }
