import {ParentComponent} from 'solid-js';

const Card: ParentComponent<{title: string}> = props => (
  <div class="p-6 bg-white border border-gray-200 rounded-lg shadow">
    <div class="font-bold text-xl mb-2">{props.title}</div>
    {props.children}
  </div>
);

export default Card