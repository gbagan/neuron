<script lang="ts">
  import Ruler from "./Ruler.svelte";
  import type { Pattern, State } from "../lib/model";
  import Calculus from "./Calculus.svelte";

  const ALPHABET = "ABCDEF";
  const COLORS = ["vert", "bleu", "rose", "jaune"];

  type Props = {
    layer: number;
    idx: number;
    state: State;
    patterns: Pattern[];
    nbStates: number;
    stateIdx: number;
    learn: () => void;
    changeCurrentState: (i: number) => void;
    changeWeight: (layer: number, i: number, j: number, val: number) => void;
    changeThreshold: (layer: number, i: number, val: number) => void;
    closeDialog: () => void;
  }

  let { layer, idx, state: st, patterns, nbStates, stateIdx, learn,
    changeCurrentState, changeWeight, changeThreshold, closeDialog }: Props = $props();

  let editMode = $state.raw(false);

  let title = $derived("Neurone " + (layer === 1 ? ALPHABET[idx] : COLORS[idx]))

  function edit() {
    editMode = !editMode;
  }
</script>

<div class="dialog-title">{title}</div>
<div class="dialog-body.flex.flex-col">
  <Calculus
    {layer}
    weights={layer === 1 ? st.hiddenWeights[idx] : st.finalWeights[idx]}
    threshold={layer === 1 ? st.hiddenThresholds[idx] : st.finalThresholds[idx]}
    {editMode}
    changeWeight={(k, val) => changeWeight(layer, idx, k, val)}
    changeThreshold={val => changeThreshold(layer, idx, val)}
  />
  <Ruler state={st} {patterns} {layer} {idx} small={false} />
  <div class="iterations">Nombre d'itérations: {st.iter}</div>
  {#if nbStates > 1}
    <input
      class="input-range"
      type="range"
      min="0"
      max={nbStates - 1}
      value={stateIdx}
      onchange={e => changeCurrentState(e.currentTarget.valueAsNumber)}
    />
  {/if}
</div>
<div class="dialog-buttons">
  <button class="btn" onclick={edit}>Editer</button>
  <button class="btn" onclick={learn}>Apprendre</button>
  <button class="btn" onclick={closeDialog}>OK</button>
</div>

<style>
  .iterations {
    font-size: 1.75rem;
  }

</style>