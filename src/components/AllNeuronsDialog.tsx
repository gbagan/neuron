import { Component, Index, Show } from "solid-js";
import Ruler from "./Ruler";
import { Pattern, State } from "../model";

type AllNeuronsComponent = Component<{
  state: State,
  patterns: Pattern[],
  nbStates: number,
  stateIdx: number,
  learn: () => void,
  changeCurrentState: (i: number) => void,
  runSimulation: () => void, 
  closeDialog: () => void,
}>

const AllNeuronsDialog: AllNeuronsComponent = props => (
  <>
    <div class="dialogtitle">Neurones de sortie</div>
    <div class="dialogbody flex flex-col ">
      <div class="grid grid-cols-2 gap-8 portrait:flex portrait:flex-col">
        <Index each={[0, 1, 2, 3]}>
          {i => (
            <Ruler
              small={true}
              layer={2}
              idx={i()}
              patterns={props.patterns}
              state={props.state}
            />
          )}
        </Index>
      </div>
      <div class="h-24 mt-12 largetext grid grid-cols-2 gap-8">
        Nombre d'itérations: {props.state.iter}
        <Show when={props.nbStates > 1}>
          <div>
            <button class="btn" onClick={props.runSimulation}>Lancer</button>
            <input
              class="inputrange range-lg"
              type="range"
              min="0"
              max={props.nbStates - 1}
              value={props.stateIdx}
              onChange={e => props.changeCurrentState(e.currentTarget.valueAsNumber)}
            />
          </div>  
        </Show>
      </div>
    </div>
    <div class="p-4 text-right">
      <button class="btn" onClick={props.learn}>Apprendre</button>
      <button class="btn ml-4" onClick={props.closeDialog}>OK</button>
    </div>
  </>
);

export default AllNeuronsDialog;
