<script lang="ts">
  const weightToString = (weight: number) => weight === 1 ? "" : weight === -1 ? "-" : weight.toFixed(3);

  type Props = {
    editMode: boolean;
    weights: readonly number[];
    threshold: number;
    layer: number;
    changeWeight: (idx: number, val: number) => void;
    changeThreshold: (val: number) => void;
  }

  let { editMode, weights, threshold, layer, changeWeight, changeThreshold }: Props = $props();
</script>

{#snippet icon(i: number)}
  <div class="icon">
    <svg>
      <use href="#neuron-{i}" />
    </svg>
  </div>
{/snippet}

<div class="calculus">
  {#if editMode}
    <span>
      {#each weights as weight, i}
        {#if i > 0}
          <span class="plus">+</span>
        {/if}
        <span>
          <input
            class="input-number"
            type="number"
            value={weight}
            onchange={e => changeWeight(i, e.currentTarget.valueAsNumber)}
          />
          {@render icon((layer === 2 ? 6 : 0) + i)}
        </span>
      {/each}
    </span>
  {:else}
    <span>
      {#each weights as weight, i}
        {#if weight !== 0}
          {i > 0 && weight > 0 ? "+" : ""}
          {weightToString(weight)}
          {@render icon((layer === 2 ? 6 : 0) + i)}
        {/if}
      {/each}
    </span>
  {/if}
  <br/>
  Seuil:
  {#if !editMode}
    {threshold}
  {:else}
    <input
      class="input-number"
      type="number"
      value={threshold}
      onchange={e => changeThreshold(e.currentTarget.valueAsNumber)}
    />
  {/if}
</div>

<style>
  .calculus {
    font-size: 2.25rem;
    line-height: 2.5rem;
  }

  .icon {
    display: inline-block;
    width: 2rem;
    height: 2rem;
  }

  svg {
    width: 100%;
    height: 100%;
  }

  .plus {
    margin-left: 1rem;
    margin-right: 1rem;
  }
</style>