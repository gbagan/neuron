module Neuron.Msg where

import Data.Maybe (Maybe)
import Neuron.Model (Dialog)

data Msg
  = SelectInput (Maybe Int)
  | SelectPattern Int
  | ChangeWeight Int Int Int String
  | ChangeThreshold Int Int String
  | OpenDialog Dialog
  | TogglePattern Int
  | ChangePixel Int
  | ResetPattern
  | ToggleEditMode
  | Simulate
  | RunLearning
  | RunSimulation
  | ChangeCurrentState String
  | Reset