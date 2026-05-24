<script lang="ts">
  import { MASK } from "../lib/model";
  import { PATTERN_COLORS } from "../lib/constants";
  import Neuron from "./Neuron.svelte";

  type Props = {
    final: number[];
    selectInput: (i: number | null) => void;
    openNeuronDialog: (layer: number, idx: number) => void;
  }

  let { final, selectInput, openNeuronDialog }: Props = $props();
</script>

{#snippet line(layer1: number, layer2: number, row1: number, row2: number)}
  <line
    y1={layer1 <= 1 ? 10 + 15 * row1 : 20 + 20 * row1}
    y2={layer2 <= 1 ? 10 + 15 * row2 : 20 + 20 * row2}
    x1={layer1 === 0 ? 15 : layer1 === 1 ? 60 : 105}
    x2={layer2 === 0 ? 15 : layer2 === 1 ? 60 : 105}
    stroke-width="0.5"
    stroke="white"
  />
{/snippet}

<svg viewBox="0 0 135 100">
  {#each MASK as row, i}
    {#each row as v, j}
      {#if v}
        {@render line(0, 1, j, i)}
      {/if}
    {/each}
  {/each}
  {#each [0, 1, 2, 3, 4, 5] as i}
    {#each [0, 1, 2, 3] as j}
      {@render line(1, 2, i, j)}
    {/each}
  {/each}
  {#each [0, 1, 2, 3, 4, 5] as i}
    <line
      y1={10 + 15 * i}
      y2={10 + 15 * i}
      x1="0"
      x2="15"
      stroke-width="0.5"
      stroke="white"
    />
  {/each}
  {#each [0, 1, 2, 3] as i}
    <line
      y1={20 + 20 * i}
      y2={20 + 20 * i}
      x1="105"
      x2="120"
      stroke-width="0.5"
      stroke="white"
    />
  {/each}
  {#each [0, 1, 2, 3, 4, 5] as i}
    <g style:transform="translate(15px, {10 + 15 * i}%)">
      <!-- svelte-ignore a11y_no_static_element_interactions -->
      <circle
        r="5"
        fill="white"
        onpointerenter={() => selectInput(i)}
        onpointerleave={() => selectInput(null)}
      />
      <Neuron idx={i} />
    </g>
  {/each}
  {#each [0, 1, 2, 3, 4, 5] as i}
    <g style:transform="translate(60px, {10 + 15 * i}%)">
      <!-- svelte-ignore a11y_no_static_element_interactions -->
      <!-- svelte-ignore a11y_click_events_have_key_events -->
      <circle
        r="5"
        fill="white"
        class="clickable"
        onclick={() => openNeuronDialog(1, i)}
      />
      <Neuron idx={i + 6} />
    </g>
  {/each}
  {#each final as value, i}
    <g style:transform="translate(105px, {20 + 20 * i}%)">
      <!-- svelte-ignore a11y_click_events_have_key_events -->
      <!-- svelte-ignore a11y_no_static_element_interactions -->
      <circle
        r="5"
        fill={PATTERN_COLORS[i]}
        class="clickable"
        onclick={() => openNeuronDialog(2, i)}
      />
      <text
        x="20"
        stroke={value > 0 ? "green" : "red"}
        font-size="0.4rem"
      >
        {value > 0 ? "✓" : "⨯"}
      </text>
    </g>
  {/each}
</svg>

<style>
  .clickable {
    cursor: pointer;
  }

  text {
    user-select: none;
  }
</style>