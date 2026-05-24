<script lang="ts">
  import Ruler from "./Ruler.svelte";
  import type { Pattern, State } from "../lib/model";

  type Props = {
    state: State;
    patterns: readonly Pattern[];
    nbStates: number;
    stateIdx: number;
    learn: () => void;
    changeCurrentState: (i: number) => void;
    runSimulation: () => void;
    closeDialog: () => void;
  }

  let { state, patterns, nbStates, stateIdx, learn, changeCurrentState, runSimulation, closeDialog }: Props = $props();
</script>

<div class="dialog-title">Neurones de sortie</div>
<div class="dialog-body body">
  <div class="rulers">
    {#each [0, 1, 2, 3] as i}
      <Ruler small={true} layer={2} idx={i} {patterns} {state} />
    {/each}
  </div>
  <div class="iterations">
    Nombre d'itérations: {state.iter}
    {#if nbStates > 1}
      <div>
        <button class="btn" onclick={runSimulation}>Lancer</button>
        <input
          class="input-range range-lg"
          type="range"
          min="0"
          max={nbStates - 1}
          value={stateIdx}
          onchange={e => changeCurrentState(e.currentTarget.valueAsNumber)}
        />
      </div>
    {/if}
  </div>
</div>

<div class="dialog-buttons">
  <button class="btn" onclick={learn}>Apprendre</button>
  <button class="btn" onclick={closeDialog}>OK</button>
</div>

<style>
  .body {
    display: flex;
    flex-direction: column;
  }

  .rulers {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 2rem;
  }

  @media (orientation: portrait) {
    .rulers{
      display: flex;
      flex-direction: column;
    }
  }

  .iterations {
    height: 6rem;
    margin-top: 3rem;
    font-size: 2.25rem;
    line-height: 2.5rem;
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 2rem;
  }

</style>