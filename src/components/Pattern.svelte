<script lang="ts">
  import { divMod } from "@gbagan/utils";
  import type { Pattern } from "../lib/model"
  
  const COLORS = ["white", "black", "lightgreen", "darkgreen"];
  
  type Props = {
    selectedCaptor: number | null;
    pattern: Pattern;
    onclick: () => void;
  };

  let { selectedCaptor, pattern, onclick }: Props = $props();
</script>

<button {onclick}>
  {#each pattern.pattern as b, i}
    {@const [row, col] = divMod(i, 6)}
    {@const list = [(row / 3 | 0) + 3, col / 2 | 0]}
    {@const color = COLORS[Number(list.includes(selectedCaptor ?? -1)) * 2 + Number(b)]}
    <div
      class="pixel"
      style:background={color}
      style:left="{col * 100 / 6}%"
      style:top="{row * 100 / 9}%"
    ></div>
  {/each}
</button>

<style>
  button {
    overflow: hidden;
    position: relative;
    border: 4px solid var(--slate-400);
    margin: 0.5rem;
    width: 18rem;
    height: 24rem;
    cursor: pointer;
  }

  .pixel {
    position: absolute;
    pointer-events: none;
    width: 16.66666666%;
    height: 11.1111111111%;
  }
</style>