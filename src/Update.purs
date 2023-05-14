module Neuron.Update
  ( simulate
  , update
  )
  where

import Prelude
import Data.Array (elem, snoc, zipWith, (..))
import Data.Foldable (sum)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int as Int
import Data.Lens ((.~), (%~))
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Neuron.Model (Model, State, Pattern
                    , _currentState, _finalThresholds, _finalWeights, _hiddenThresholds, _hiddenWeights
                    , _patterns, _states, _selected
                    , init, initPatterns, mask, countPixels)
import Neuron.Msg (Msg(..))
import Neuron.Util (map2, (!))
import Pha.Update (Update, get, modify_, put, delay)
import Type.Proxy (Proxy(..))

-- | nombre d'itérations pour l'apprentissage
nbIters :: Int
nbIters = 60000

-- | itérations clés à retenir pour l'apprentissage
iterList :: Array Int
iterList = [0, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 40000, 50000, 60000]

-- | désapprend et garde uniquement un état, celui qui était l'état courant
keepOneState :: Model -> Model
keepOneState model = 
      model # _states %~ (\sts -> [(sts ! model.currentState){iter = 0}])
            # _currentState .~ 0


-- | calcule la valeur des neuronnes d'entrée à partir d'une liste de patterns
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

foreign import runStepImpl :: Array (Array Boolean) -> Model -> State -> State

runStep :: Model -> State -> State
runStep m@{ inputs } st = updateOutput inputs $ runStepImpl mask m st

runLearning :: Model -> Model
runLearning m@{ states } = m { states = go 0 [] (states ! 0) }
  where
  go n acc st | n==nbIters = acc `snoc` st
              | n `elem` iterList = go (n + 1) (acc `snoc` st) (runStep m st)
              | otherwise = go (n+1) acc (runStep m st)


simulate :: Model -> Model
simulate model@{ states, patterns } = model { inputs = inputs, states = states <#> updateOutput inputs }
  where
  inputs = updateInput patterns


update :: Msg -> Update Model Msg Aff Unit
update msg = case msg of
  SelectInput i -> modify_ _{ selectedInput = i }

  SelectPattern i -> modify_ $ simulate <<< _{ currentPattern = i }

  ChangeWeight i j k s -> case Number.fromString s of
    Nothing -> pure unit
    Just val ->
      modify_ $ simulate <<< keepOneState <<< \model -> model #
        ( _states <<< ix model.currentState <<< (if i == 1 then _hiddenWeights else _finalWeights) <<< ix j <<< ix k
        ) .~ val

  ChangeThreshold i j s -> case Number.fromString s of
    Nothing -> pure unit
    Just val ->
      modify_ $ simulate <<< keepOneState <<< \model -> model #
        ( _states <<< ix model.currentState <<< (if i == 1 then _hiddenThresholds else _finalThresholds) <<< ix j
        ) .~ val

  OpenDialog d -> modify_ _{ dialog = d }

  TogglePattern i -> modify_ $ (_patterns <<< ix i <<< _selected) %~ not

  ChangePixel i -> modify_ $ simulate <<< \model -> model #
    ( _patterns
        <<< ix model.currentPattern
        <<< prop (Proxy :: _ "pattern")
        <<< ix i
    ) %~ not

  ToggleEditMode -> modify_ \model -> model { editMode = not model.editMode }

  ResetPattern -> modify_ $ simulate <<< \model -> model # (_patterns <<< ix model.currentPattern) .~
    (initPatterns ! model.currentPattern)

  RunLearning -> modify_ runLearning

  RunSimulation -> do
    model <- get
    forWithIndex_ model.states \i _ -> do
      modify_ _{currentState = i}
      delay $ Milliseconds 2000.0

  ChangeCurrentState s -> case Int.fromString s of
    Nothing -> pure unit
    Just i -> modify_ _{ currentState = i }

  Reset -> put $ simulate init
