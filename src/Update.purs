module Neuron.Update where

import Prelude
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int as Int
import Data.Lens ((.~), (%~))
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Neuron.Model (Model, _currentState, _finalThresholds, _finalWeights, _hiddenThresholds, _hiddenWeights
                    , _patterns, _states, _selected
                    , init, initPatterns, runLearning, simulate)
import Neuron.Msg (Msg(..))
import Neuron.Util ((!))
import Pha.Update (Update, get, modify_, put, delay)
import Type.Proxy (Proxy(..))

update :: Msg -> Update Model Aff Unit
update msg = case msg of
  SelectInput i -> modify_ _{ selectedInput = i }
  SelectPattern i -> modify_ $ simulate <<< _{ currentPattern = i }
  ChangeWeight i j k s -> case Number.fromString s of
    Nothing -> pure unit
    Just val ->
      modify_ $ simulate <<< \model -> model #
        ( _states <<< ix model.currentState <<< (if i == 1 then _hiddenWeights else _finalWeights) <<< ix j <<< ix k
        ) .~ val
        # _states %~ (\sts -> [(sts ! model.currentState){iter = 0}])
        # _currentState .~ 0
  ChangeThreshold i j s -> case Number.fromString s of
    Nothing -> pure unit
    Just val ->
      modify_ $ simulate <<< \model -> model #
        ( _states <<< ix model.currentState <<< (if i == 1 then _hiddenThresholds else _finalThresholds) <<< ix j
        ) .~ val
        # _states %~ (\sts -> [(sts ! model.currentState){iter = 0}])
        # _currentState .~ 0
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
  Reset -> put init
