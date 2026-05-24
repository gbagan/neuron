<script lang="ts">
  import type { Pattern } from "../lib/model";
  import MiniPattern from "./MiniPattern.svelte";

  type Props = {
    patterns: readonly Pattern[]
    currentPattern: number
    setCurrentPattern: (i: number) => void;
    togglePattern: (i: number) => void;
    resetPatterns: () => void;
    changePixel: (i: number) => void;
    closeDialog: () => void;
  }

  let { patterns, currentPattern, setCurrentPattern, 
    togglePattern, resetPatterns, changePixel, closeDialog }: Props = $props();
</script>

<div class="dialog-title">Modifier le motif</div>
<div class="dialog-body body">
  <div class="current-pattern">
    {#each patterns[currentPattern].pattern as b, i}
      <button
        class="pixel"
        aria-label="pixel"
        style:background={b ? "black" : "white"}
        style:left="{100 * (i % 6) / 6}%"
        style:top="{100 * (i / 6 | 0) / 9}%"
        onclick={() => changePixel(i)}
      ></button>
    {/each}
  </div>
  <div class="patterns">
    {#each patterns as pattern, i}
      <div class="pattern">
        <label class="label">
          <input
            type="checkbox"
            checked={pattern.selected}
            class="checkbox"
            onchange={() => togglePattern(i)}
          />
          <div class="slider"></div>
        </label>
        <MiniPattern
          {pattern}
          isCurrent={i === currentPattern}
          onclick={() => setCurrentPattern(i)}
        />
      </div>
    {/each}
  </div>
</div>
<div class="dialog-buttons">
  <button class="btn" onclick={resetPatterns}>Réinitialiser</button>
  <button class="btn" onclick={closeDialog}>OK</button>
</div>

<style>
  .body {
    display: flex;
    flex-direction: row;
    align-items: center;
    gap: 2rem;
  }

  @media (orientation: portrait) {
    .body {
      flex-direction: column;
    }
  }

  .current-pattern {
    margin: 0.5rem;
    overflow: hidden;
    position: relative;
    background-color: #ffffff;
    border: 4px solid;
    width: 16rem;
    height: 24rem;
  }

  .pixel {
    position: absolute;
    width: 16.66666666%;
    height: 11.1111111111%;
  }

  .patterns {
    display: grid;
    grid-template-columns: repeat(6, minmax(0, 1fr));
    gap: 0.25rem;
  }

  .pattern {
    display: flex;
    flex-direction: row;
    align-items: center;
  }

  .label {
    position: relative;
    display: inline-flex;
    align-items: center;
    cursor: pointer;
  }

  .checkbox {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border-width: 0;
  }

  .slider {
    position: relative;
    width: 2.75rem;
    height: 1.5rem;
    background-color: #374151;
    border-color: #4b5563;
    border-radius: 9999px;
    transform: rotate(90deg);
  }

  input:focus ~ .slider {
    outline: none;
    box-shadow: 0 0 0 4px rgb(37 99 235 / 0.5);
  }

  input:checked ~ .slider::after {
    transform: translateX(100%);
    border-color: #ffffff;
  }

  input:checked ~ .slider {
    background-color: var(--blue-600);
  }

  .slider::after {
    content: '';
    position: absolute;
    top: 2px;
    left: 2px;
    background-color: #ffffff;
    border: 1px solid var(--gray-300);
    border-radius: 9999px;
    height: 1.25rem;
    width: 1.25rem;
    transition: all 150ms ease-in-out;
  }
</style>