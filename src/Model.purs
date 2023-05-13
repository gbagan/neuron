module Neuron.Model where

import Prelude

import Data.Array ((..), elem, filter, mapWithIndex, snoc, sortWith)
import Data.Foldable (foldl, minimum, maximum)
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

--- lenses
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

-- | état initial, pas utilisable dans l'état actuel. On doit appliquer `simulate` dessus
-- | pour avoir un modèle cohérent
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
  }

-- | compte le nombre de pixels que capte le neurone d'entrée i sur un pattern donné
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

-- | ```haskell
-- | firstAvailableHeight [0, 1, 2, 4, 6, 7] = 3
-- | ````
firstAvailableHeight :: Array Int -> Int
firstAvailableHeight xs = go 0 where
  go x | x `elem` xs = go (x+1)
       | otherwise = x

type RulerPositions = 
  { zero :: Number
  , symbols :: Array { symbol :: Int, x :: Number, y :: Number }
  , graduation :: Array { value :: Int, x :: Number }
  }

-- | Calcule un ensemble d'information utile pour dessiner une réglette.
-- | Les positions horizontales sont compris dans l'intervalle [0, 1]
-- | `zero` indique la position horizontal de zero
-- | todo à finir
rulerPositions :: Array Pattern -> State -> Int -> Int -> RulerPositions                    
rulerPositions patterns st i j =
  let
    values = st.output <#> \{hidden, final} -> if i == 1 then hidden ! j else final ! j
    minX = fromMaybe 0.0 $ minimum values
    maxX = fromMaybe 0.0 $ maximum values
    values' = values <#> \v -> (v - minX) / (maxX - minX)
    symbols = map2 patterns values' (\{selected, symbol} value -> {selected, symbol, value})
          # filter _.selected
          # map (\{symbol, value} -> {symbol, value})
          # sortWith _.value
          # foldl go []
          # map (\s -> s{y = Int.toNumber s.y})
    go acc {symbol, value} =
      let previousHeights = acc
                    # filter (\{x} -> value - x <= 0.049)
                    # map _.y
          y = firstAvailableHeight previousHeights
      in
        acc `snoc` {symbol, x: value, y}
  in
    { zero: - minX / (maxX - minX)
    , symbols
    , graduation: floor minX .. ceil maxX <#> \k -> {value: k, x: (Int.toNumber k - minX) / (maxX - minX)}
    }
