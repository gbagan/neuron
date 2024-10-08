import { Component } from "solid-js";

const Neuron: Component<{idx: number}> = props =>
  <use
    href={`#neuron-${props.idx}`}
    x="-3.0"
    y="-3.0"
    width="6"
    height="6"
    class="pointer-events-none select-none"
  />

export default Neuron;