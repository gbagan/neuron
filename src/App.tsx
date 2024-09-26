import { Component, Match, Switch, batch } from 'solid-js';
import { createStore, produce } from "solid-js/store";
import { initModel, updateInput, updateOutput } from './model';
import PatternView from './components/Pattern';
import Network from './components/Network';
import Editor from './components/Editor';
import { runLearning } from './learn';
import AllNeuronsDialog from './components/AllNeuronsDialog';

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

  const openEditorDialog = () => {
    setModel("dialog", {type: "edit"});
    dialog.showModal();
  }

  const openAllNeuronsDialog = () => {
    setModel("dialog", {type: "neurons"});
    dialog.showModal();
  }

  const closeDialog = () => {
    dialog.close()  
  }

  const selectInput = (i: number | null) => setModel("selectedInput", i);

  const togglePattern = (i: number) => setModel("patterns", i, "selected", b => !b);

  const setCurrentPattern = (i: number) => setModel("currentPattern", i);

  const changePixel = (i: number) => {
    batch(() => {
      setModel("patterns", model.currentPattern, "pattern", i, b => !b);
      simulate();
    })
  }

  const learn = () => {
    setModel("states", runLearning(model));
  }

  const changeCurrentState = (i: number) => {
    setModel("currentState", i);
  }

  const reset = () => {
    setModel(initModel());
  }

  const editorActions = {
    setCurrentPattern,
    changePixel,
    closeDialog,
    togglePattern,
  }

  const networkActions = {
    selectInput,
  }

  return (
    <div class="w-full min-h-screen bg-black">
      <div class="flex flew-row">
        <div class="w-1/4">
          <PatternView
            selectedCaptor={model.selectedInput}
            pattern={model.patterns[model.currentPattern]}
            onClick={openEditorDialog}
          />
          <button class="btn" onClick={openAllNeuronsDialog}>Afficher les réglettes</button>
          <button class="btn" onClick={reset}>Réinitialiser</button>
        </div>
        <div class="w-3/4 relative">
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
              learn={learn}
              changeCurrentState={changeCurrentState}
              closeDialog={closeDialog}
            />
          </Match>
        </Switch>
      </dialog>
    </div>
  )
}

export default App;