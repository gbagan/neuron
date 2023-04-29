module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Pha.App (app)
import Neuron.Model (init)
import Neuron.Msg (Msg(..))
import Neuron.Update (update)
import Neuron.View (view)

main :: Effect Unit
main = app { init: { state: init, action: Just Simulate}
           , eval: identity
           , view
           , update
           , subscriptions: []
           , selector: "#root"
           }
