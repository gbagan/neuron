<script lang="ts">
  import type { Pattern } from "../lib/model";
  import { PATTERN_COLORS } from "../lib/constants";

  type Props = {
    pattern: Pattern;
    isCurrent: boolean;
    onclick: () => void;
  }

  let { pattern, isCurrent, onclick}: Props = $props();
</script>

<button {onclick} class={isCurrent ? "current" : "notcurrent"}>
  {#each pattern.pattern as b, i}
    <div
      class="pixel"
      style:background-color={b ? PATTERN_COLORS[pattern.symbol] : "white"}
      style:left="{(i % 6) * 100 / 6}%"
      style:top="{(i / 6 | 0) * 100  / 9}%"
    ></div>
  {/each}
</button>

<style>
  button {
    overflow: hidden;
    position: relative;
    background-color: #ffffff;
    border: 4px solid #94a3b8;
    margin: 0.5rem;
    width: 5rem;
    height: 7.5rem;
    cursor: pointer;
  }

  .pixel {
    position: absolute;
    width: 16.66666666%;
    height: 11.1111111111%;
  }

  .current {
    border-color: var(--green-500);
  }

  .notcurrent {
    border-color: var(--slate-400);
  }
</style>