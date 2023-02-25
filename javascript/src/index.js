import {
  Env,
} from './functions';
const assert = console.assert;

export default function cippo(x, y) {
  return x + 2*y;
}

const pipo = Env({ w: 123 });


assert(pipo.insert('qwe')(234).lookup('w') === 123, 'Env not working');












// debugger;
