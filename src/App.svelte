<script lang="ts">
  import { set, sleep, update } from '@gbagan/utils';
  import { type Dialog, initPatterns, initState, type Matrix, type State, updateInput, updateOutput } from './lib/model';
  import { runLearning } from './lib/learn';
  import PatternView from './components/Pattern.svelte';
  import Network from './components/Network.svelte';
  import Editor from './components/Editor.svelte';
  import AllNeuronsDialog from './components/AllNeuronsDialog.svelte';
  import NeuronDialog from './components/NeuronDialog.svelte';

  let patterns = $state.raw(initPatterns);
  let inputs: Matrix = $state.raw([]);
  let states: State[] = $state.raw([initState]);
  let currentState = $state.raw(0);
  let currentPattern = $state.raw(0);
  let selectedInput: number | null = $state.raw(null);
  let dialog: Dialog = $state.raw({ type: "none" });

  let dialogEl!: HTMLDialogElement;

  let output = $derived(states[currentState].output);
  let final = $derived(output[currentPattern].final);

  function simulate() {
    inputs = updateInput(patterns);
    states = states.map(st => ({...st, output: updateOutput(inputs, st)}));
  }

  function keepOneState() {
    let state = states[currentState];
    state = {...state, iter: 0};
    states = [state];
    currentState = 0;
  }

  function openEditorDialog() {
    dialog = {type: "edit"};
    dialogEl.showModal()
  }

  function openAllNeuronsDialog() {
    dialog = {type: "neurons"};
    dialogEl.showModal();
  }

  function openNeuronDialog(layer: number, idx: number) {
    dialog = {type: "neuron", layer, idx};
    dialogEl.showModal();
  }

  function closeDialog() {
    dialog = { type: "none" };
    dialogEl.close();
  }

  function selectInput(i: number | null) {
    selectedInput = i;
  }

  function togglePattern(i: number) {
    patterns = update(patterns, [i, "selected"], x => !x); 
  }
  
  function setCurrentPattern(i: number) {
    currentPattern = i;
  }

  function previousPattern() {
    currentPattern = (currentPattern + 23) % 24;
  }
  
  function nextPattern() {
    currentPattern = (currentPattern + 1) % 24;
  }

  function changePixel(i: number) {
    patterns = update(patterns, [currentPattern, "pattern", i], x => !x);
    simulate();
  }

  function resetPatterns() {
    patterns = initPatterns;
    simulate();
  }

  function changeWeight(layer: number, i: number, j: number, val: number) {
    states = set(states, [currentState, layer === 1 ? "hiddenWeights" : "finalWeights", i, j], val);
    keepOneState();
    simulate();
  }

  function changeThreshold(layer: number, i: number, val: number) {
    states = set(states, [currentState, layer === 1 ? "hiddenThresholds" : "finalThresholds", i], val);
    keepOneState();
    simulate();
  }

  function learn() {
    states = runLearning(inputs, states[0], patterns);
  }

  function changeCurrentState(i :number) {
    currentState = i;
  }

  function reset() {
    patterns = initPatterns;
    inputs = [];
    states = [initState];
    currentState = 0;
    currentPattern = 0;
    selectedInput = null;
    dialog = { type: "none" };
    simulate();
  }

  async function runSimulation() {
    const n = states.length;
    for (let i = 0; i < n; i++) {
      currentState = i;
      await sleep(1500);
    }
  }

  simulate();
</script>

<div class="app">
  <aside>
    <PatternView
      selectedCaptor={selectedInput}
      pattern={patterns[currentPattern]}
      onclick={openEditorDialog}
    />
    <div class="buttons">
      <button class="btn" onclick={previousPattern}>Précédent</button>
      <button class="btn" onclick={nextPattern}>Suivant</button>
      <button class="btn" onclick={openAllNeuronsDialog}>Réglettes</button>
      <button class="btn" onclick={reset}>Réinitialiser</button>
    </div>
  </aside>
  <main>
    <Network {final} {selectInput} {openNeuronDialog} />
  </main>
</div>
  
<dialog class="dialog" bind:this={dialogEl}>
  {#if dialog.type === "edit"}
    <Editor {patterns} {currentPattern} 
      {setCurrentPattern} {changePixel} {closeDialog} {togglePattern} {resetPatterns}
    />
  {:else if dialog.type === "neurons"}
    <AllNeuronsDialog
      state={states[currentState]}
      {patterns}
      nbStates={states.length}
      stateIdx={currentState}
      {learn}
      {changeCurrentState}
      {runSimulation}
      {closeDialog}
    />
  {:else if dialog.type === "neuron"}
    <NeuronDialog
      layer={dialog.layer}
      idx={dialog.idx}
      state={states[currentState]}
      {patterns}
      nbStates={states.length}
      stateIdx={currentState}
      {learn}
      {changeCurrentState}
      {changeThreshold}
      {changeWeight}
      {closeDialog}
    />
  {/if}
</dialog>

<style>
  .app {
    width: 100%;
    min-height: 100vh;
    background-color: var(--bg);
    display: flex;
    flex-direction: row;
    align-items: center;
    justify-content: space-around;
  }

  aside {
    display: flex;
    flex-direction: column;
    align-items: center;
  }

  main {
    width: 66.666667%;
  }

  @media (orientation: portrait) {
    .app {
      flex-direction: column;
    }

    aside {
      flex-direction: row;
    }

    main {
      width: 100%;
    }
  }

  .buttons {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 0.25rem;
  }
</style>