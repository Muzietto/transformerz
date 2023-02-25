import {
  Env,
} from './functions';
import {
  Var,
  Lit,
} from './expressions';
import {
  IntVal,
} from './values';
import {
  eval0,
} from './Nilsson_01';
const assert = console.assert;

export default function cippo(x, y) {
  return x + 2*y;
}

const pipo = Env({ w: 123 });
assert(pipo.insert('qwe')(234).lookup('w').isJust, 'Env not working1');
assert(pipo.insert('qwe')(234).lookup('w').get() === 123, 'Env not working2');

const lit12 = Lit(12);
assert(lit12.i === 12, 'Lit not working1');
assert(lit12.isLit, 'Lit not working2');
assert(lit12.isExp, 'Lit not working3');

const varXxxx = Var('xxxx');
assert(varXxxx.name === 'xxxx', 'Var not working1');
assert(varXxxx.isVar, 'Var not working2');
assert(lit12.isExp, 'Var not working3');

const intVal12 = IntVal(12);
assert(intVal12.i === 12, 'IntVal not working1');
assert(intVal12.isIntVal, 'IntVal not working2');
assert(intVal12.isValue, 'IntVal not working3');

const eval0Var = eval0(pipo)(Var('w'));
assert(eval0Var === 123, 'eval0 not working11');

const eval0Lit = eval0(pipo)(Lit(234));
assert(eval0Lit.isIntVal, 'eval0 not working21');
assert(eval0Lit.i === 234, 'eval0 not working22');




if (typeof window !== 'undefined') {
  window.Env = Env;
  window.Lit = Lit;
  window.Var = Var;
  window.IntVal = IntVal;
}
