module Neuron.Model where

import Prelude

import Data.Array ((..), filter, mapWithIndex, snoc, sortWith, zipWith)
import Data.Foldable (foldl, sum, minimum, maximum)
import Data.Int (floor, ceil)
import Data.Int as Int
import Data.Lens (Lens', (.~), (%~))
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Neuron.Patterns
  ( pattern01
  , pattern02
  , pattern03
  , pattern04
  , pattern31
  , pattern32
  , pattern33
  , pattern34
  , pattern61
  , pattern62
  , pattern63
  , pattern64
  , pattern91
  , pattern92
  , pattern93
  , pattern94
  , emptyPattern
  )
import Neuron.Util (count, (!), map2)
import Type.Proxy (Proxy(..))

type Pattern = { symbol :: Int, pattern :: Array Boolean, selected :: Boolean }

data Dialog = EditorDialog | NeuronDialog Int Int | NoDialog

type Value = { value :: Number, cut :: Number }

type State =
  { hiddenThresholds :: Array Number
  , hiddenWeights :: Array (Array Number)
  , finalThresholds :: Array Number
  , finalWeights :: Array (Array Number)
  , output :: Array { hidden :: Array Value, final :: Array Number }
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

delta :: Array (Array Boolean)
delta =
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
        let
          s = sum (zipWith (*) input hw) - t
        in
          if s < 0.0 then
            { value: 0.0, cut: 1.0 }
          else
            { value: s, cut: 0.0 }
      final = map2 st.finalThresholds st.finalWeights \t fw ->
        sum (zipWith (*) (_.value <$> hidden) fw) - t
    in
      { final, hidden }

simulate :: Model -> Model
simulate model@{ states, patterns } = model { inputs = inputs, states = states <#> updateOutput inputs }
  where
  inputs = updateInput patterns

foreign import runStepImpl :: Array (Array Boolean) -> Model -> State -> State

runStep :: Model -> State -> State
runStep m@{ inputs } st = updateOutput inputs $ runStepImpl delta m st

runLearning :: Int -> Model -> Model
runLearning nsteps m@{ states } = m { states = [ go nsteps (states ! 0) ] }
  where
  go 0 st = st
  go n st = go (n - 1) (runStep m st)

data Msg
  = SelectInput (Maybe Int)
  | SelectPattern Int
  | ChangeHiddenWeight Int Int String
  | ChangeThreshold Int String
  | OpenDialog Dialog
  | ChangePixel Int
  | ResetPattern
  | ToggleEditMode
  | RunLearning

update :: Msg -> Model -> Model
update msg model = case msg of
  SelectInput i -> model { selectedInput = i }
  SelectPattern i -> simulate $ model { currentPattern = i }
  ChangeHiddenWeight i j s -> case Number.fromString s of
    Nothing -> model
    Just val ->
      simulate $ model #
        ( _states <<< ix 0 <<< _hiddenWeights <<< ix i <<< ix j
        ) .~ val
  ChangeThreshold i s -> case Number.fromString s of
    Nothing -> model
    Just val ->
      simulate $ model #
        ( _states <<< ix 0 <<< _hiddenThresholds <<< ix i
        ) .~ val
  OpenDialog d -> model { dialog = d }
  ChangePixel i -> simulate $ model #
    ( _patterns
        <<< ix model.currentPattern
        <<< prop (Proxy :: _ "pattern")
        <<< ix i
    ) %~ not
  ToggleEditMode -> model { editMode = not model.editMode }
  ResetPattern -> simulate $ model # (_patterns <<< ix model.currentPattern) .~
    (initPatterns ! model.currentPattern)
  RunLearning -> runLearning 100 model


rulerPositions :: Array Pattern -> State -> Int -> Int ->
                    { zero :: Number
                    , symbols :: Array { symbol :: Int, x :: Number, y :: Number }
                    , graduation :: Array { value :: Int, x :: Number }
                    }
rulerPositions patterns st i j =
  let
    values = st.output <#> \{hidden, final} -> if i == 1 then (hidden ! j).value else final ! j
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
