<script lang="ts">
  type Props = {
    idx: number;
  }

  let { idx }: Props = $props();
</script>

<use
  href="#neuron-{idx}"
  x="-3.0"
  y="-3.0"
  width="6"
  height="6"
/>


<style>
  use {
    pointer-events: none;
    user-select: none;
  }
</style>