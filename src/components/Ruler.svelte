<script lang="ts">
  import { type Pattern, rulerPositions, type State } from "../lib/model"
  import { PATTERN_COLORS } from "../lib/constants";

  type Props = {
    patterns: readonly Pattern[],
    state: State,
    layer: number,
    idx: number,
    small: boolean,
  }

  let { patterns, state, layer, idx, small }: Props = $props();

  let { zero, graduation, symbols } = $derived(rulerPositions(patterns, state, layer, idx))
</script>

<div class={["ruler", {small}]}>
  <svg viewBox="-10 0 120 30">
    <rect x="-10" y="0" width="120" height="40" fill="#b0ffb0" />
    <rect x="-10" y="0" width={7.5 + zero * 100} height="40" fill="#ffb0b0" />
    {#each graduation as {x}}
      <line
        x1={100*x-2.5}
        x2={100*x-2.5}
        y1="0"
        y2="30"
        stroke-width="0.2"
        stroke="#808080"
        stroke-dasharray="0.5"
      />
    {/each}
    {#each symbols as {x, y, symbol}}
      <g style:transform="translate({100 * x}px, {25 - 5 * y}px)">
        <rect x="-2.5" y="0" width="5" height="5" fill={PATTERN_COLORS[symbol]} />
        <text x="-1" y="4.5" font-size="0.3rem">{symbol * 3}</text>
      </g>
    {/each}
  </svg>
</div>

<style>
  .ruler {
    border: solid 1px;
    width: 75rem;
    &.small {
      width: 40rem;
    }
  }
</style>