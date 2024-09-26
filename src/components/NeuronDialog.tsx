import { Component, createSignal, Index, Show } from "solid-js";
import Ruler from "./Ruler";
import { Pattern, State } from "../model";
import Calculus from "./Calculus";

const alphabet = "ABCDEF";
const colors = ["vert", "bleu", "rose", "jaune"];

const title = (layer: number, i: number) => "Neurone " + (layer === 1 ? alphabet[i] : colors[i]);

type NeuronComponent = Component<{
  layer: number,
  idx: number,
  state: State,
  patterns: Pattern[],
  nbStates: number,
  stateIdx: number,
  learn: () => void,
  changeCurrentState: (i: number) => void,
  changeWeight: (layer: number, i: number, j: number, val: number) => void,
  changeThreshold: (layer: number, i: number, val: number) => void,
  closeDialog: () => void,
}>

const NeuronDialog: NeuronComponent = props => {
  const [editMode, setEditMode] = createSignal(false);
  
  const edit = () => setEditMode(b => !b);

  return (
    <>
      <div class="dialog-title">{title(props.layer, props.idx)}</div>
      <div class="dialog-body flex flex-col">
        <Calculus
          layer={props.layer}
          weights={props.layer === 1 ? props.state.hiddenWeights[props.idx] : props.state.finalWeights[props.idx]}
          threshold={props.layer === 1 ? props.state.hiddenThresholds[props.idx] : props.state.finalThresholds[props.idx]}
          editMode={editMode()}
          changeWeight={(k, val) => props.changeWeight(props.layer, props.idx, k, val)}
          changeThreshold={val => props.changeThreshold(props.layer, props.idx, val)}
        />
        <Ruler
          state={props.state}
          patterns={props.patterns}
          layer={props.layer}
          idx={props.idx}
          small={false}
        />
        <div class="largetext">Nombre d'itérations: {props.state.iter}</div>
        <Show when={props.nbStates > 1}>
          <input
            class="inputrange range-lg"
            type="range"
            min="0"
            max={props.nbStates - 1}
            value={props.stateIdx}
            onChange={e => props.changeCurrentState(e.currentTarget.valueAsNumber)}
          />
        </Show>
      </div>
      <div class="dialog-buttons">
        <button class="btn" onClick={edit}>Editer</button>
        <button class="btn" onClick={props.learn}>Apprendre</button>
        <button class="btn" onClick={props.closeDialog}>OK</button>
      </div>
    </>
  )
}

export default NeuronDialog;