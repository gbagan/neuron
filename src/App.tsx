import { Component, Match, Switch, batch } from 'solid-js';
import { createStore, produce } from "solid-js/store";
import { initModel, initPatterns, NDialog, updateInput, updateOutput } from './model';
import { runLearning } from './learn';
import PatternView from './components/Pattern';
import Network from './components/Network';
import Editor from './components/Editor';
import AllNeuronsDialog from './components/AllNeuronsDialog';
import NeuronDialog from './components/NeuronDialog';
import { delay } from './util';

const App: Component = () => {
  const [model, setModel] = createStore(initModel());
  let dialog: HTMLDialogElement;

  const output = () => model.states[model.currentState].output;
  const final = () => output()[model.currentPattern].final;

  const simulate = () => {
    setModel(produce(model => {
      model.inputs = updateInput(model.patterns);
      for (const st of model.states) {
        st.output = updateOutput(model.inputs, st);
      }
    }))
  }

  simulate();

  const keepOneState = () => {
    setModel("states", [model.states[model.currentState]]);
    setModel("states", 0, "iter", 0);
    setModel("currentState", 0);
  }


  const openEditorDialog = () => {
    setModel("dialog", {type: "edit"});
    dialog.showModal();
  }

  const openAllNeuronsDialog = () => {
    setModel("dialog", {type: "neurons"});
    dialog.showModal();
  }

  const openNeuronDialog = (layer: number, idx: number) => {
    setModel("dialog", {type: "neuron", layer, idx});
    dialog.showModal();
  }

  const closeDialog = () => {
    setModel("dialog", {type :"none"});
    dialog.close()  
  }

  const selectInput = (i: number | null) => setModel("selectedInput", i);
  const togglePattern = (i: number) => setModel("patterns", i, "selected", b => !b);
  const setCurrentPattern = (i: number) => setModel("currentPattern", i);
  const previousPattern = () => setModel("currentPattern", i => (i + 23) % 24)
  const nextPattern = () => setModel("currentPattern", i => (i + 1) % 24)

  const changePixel = (i: number) => {
    batch(() => {
      setModel("patterns", model.currentPattern, "pattern", i, b => !b);
      simulate();
    })
  }

  const resetPatterns = () => {
    batch(() => {
      setModel("patterns", initPatterns());
      simulate();
    })
  }

  const changeWeight = (layer: number, i: number, j: number, val: number) => {
    batch(() => {
      setModel("states", model.currentState, layer == 1 ? "hiddenWeights" : "finalWeights", i, j, val);
      keepOneState();
      simulate();
    });
  }

  const changeThreshold = (layer: number, i: number, val: number) => {
    batch(() => {
      setModel("states", model.currentState, layer == 1 ? "hiddenThresholds" : "finalThresholds", i, val);
      keepOneState();
      simulate();
    });
  }

  const learn = () => {
    setModel("states", runLearning(model));
  }

  const changeCurrentState = (i: number) => {
    setModel("currentState", i);
  }

  const reset = () => {
    batch(() => {
      setModel(initModel());
      simulate();
    })
  }

  const runSimulation = async () => {
    const n = model.states.length;
    for (let i = 0; i < n; i++) {
      setModel("currentState", i);
      await delay(1500);
    }
  }

  const editorActions = {
    setCurrentPattern,
    changePixel,
    closeDialog,
    togglePattern,
    resetPatterns,
  }

  const networkActions = {
    selectInput,
    openNeuronDialog
  }

  const neuronDialogActions = {
    learn,
    changeCurrentState,
    changeThreshold,
    changeWeight,
    closeDialog,
  }

  const allNeuronsActions = {
    learn,
    changeCurrentState,
    runSimulation,
    closeDialog,
  }

  return (
    <>
      <div class="w-full min-h-screen bg-black flex flew-row items-start justify-around portrait:flex-col">
        <div class="flex flex-col items-center portrait:flex-row">
          <PatternView
            selectedCaptor={model.selectedInput}
            pattern={model.patterns[model.currentPattern]}
            onClick={openEditorDialog}
          />
          <div class="grid grid-cols-2 gap-1">
            <button class="btn" onClick={previousPattern}>Précédent</button>
            <button class="btn" onClick={nextPattern}>Suivant</button>
            <button class="btn" onClick={openAllNeuronsDialog}>Réglettes</button>
            <button class="btn" onClick={reset}>Réinitialiser</button>
          </div>      
        </div>
        <div class="w-3/4 relative portrait:w-full">
          <Network
            final={final()}
            {...networkActions}
          />
        </div>
      </div>
      <dialog class="dialog" ref={el => (dialog = el)}>
        <Switch>
          <Match when={model.dialog.type === "edit"}>
            <Editor
              patterns={model.patterns}
              currentPattern={model.currentPattern}
              {...editorActions}
            />
          </Match>
          <Match when={model.dialog.type === "neurons"}>
            <AllNeuronsDialog
              state={model.states[model.currentState]}
              patterns={model.patterns}
              nbStates={model.states.length}
              stateIdx={model.currentState}
              {...allNeuronsActions}
            />
          </Match>
          <Match when={model.dialog.type === "neuron"}>
            <NeuronDialog
              layer={(model.dialog as NDialog).layer}
              idx={(model.dialog as NDialog).idx}
              state={model.states[model.currentState]}
              patterns={model.patterns}
              nbStates={model.states.length}
              stateIdx={model.currentState}
              {...neuronDialogActions}
            />
          </Match>
        </Switch>
      </dialog>
    </>
  )
}

export default App;