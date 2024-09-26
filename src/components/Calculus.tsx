import { Component, Index, Match, Show, Switch } from "solid-js";
import { Pattern, State } from "../model";
import Neuron from "./Neuron";

const weightToString = (weight: number) =>
  weight === 1 ? "" : weight === -1 ? "-" : weight.toFixed(3); 

type CalculusComponent = Component<{
  editMode: boolean,
  weights: number[],
  threshold: number,
  layer: number,
  changeWeight: (idx: number, val: number) => void
  changeThreshold: (val: number) => void
}>

const Calculus: CalculusComponent = props => (
  <div class="text-4xl">
    <Switch>
      <Match when={props.editMode}>
        <span>
          <Index each={props.weights}>
            {(weight, i) => (
              <>
                <Show when={i > 0}>
                  <span class="text-4xl mx-4">+</span>
                </Show>
                <span>
                  <input
                    class="inputnumber"
                    type="number"
                    value={weight()}
                    onChange={e => props.changeWeight(i, e.currentTarget.valueAsNumber)}
                  />
                  <div class="inline-block w-8 h-8">
                    <svg class="w-full h-full">
                      <use href={`#neuron-${(props.layer === 2 ? 6 : 0) + i}`} />
                    </svg>
                  </div>
                </span>
                
              </>
            )}
          </Index>
        </span>
      </Match>
      <Match when={true}>
        <span>
          <Index each={props.weights}>
            {(weight, i) => (
              <Show when={weight() !== 0}>
                <>
                  {i > 0 && weight() > 0 ? "+" : ""}
                  {weightToString(weight())}
                  <div class="inline-block w-8 h-8">
                    <svg class="w-full h-full">
                      <use href={`#neuron-${(props.layer === 2 ? 6 : 0) + i}`} />
                    </svg>
                  </div>
                </>
              </Show>
            )}
          </Index>
        </span>
      </Match>
    </Switch>
    <br/>
    Seuil: 
    <Show when={props.editMode} fallback={""+props.threshold}>
      <input
        class="inputnumber"
        type="number"
        value={props.threshold}
        onChange={e => props.changeThreshold(e.currentTarget.valueAsNumber)}
      />
    </Show>
  </div> 
)

export default Calculus;