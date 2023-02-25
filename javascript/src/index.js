import {
  Env,
} from '@src/functions';
import {
  Var,
  Lit,
  Plus,
  Lambda,
} from '@src/expressions';
import {
  IntVal,
  FunVal,
} from '@src/values';
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

const xPlusY = Plus(Lit(1))(Lit(2));
assert(xPlusY.e1.i === 1, 'Plus not working1');
assert(xPlusY.e2.i === 2, 'Plus not working2');
assert(xPlusY.isPlus, 'Plus not working3');

const identity = Lambda('x')(Var('x'));
assert(identity.argname === 'x', 'Lambda not working0');
assert(identity.body.isVar, 'Lambda not working1');
assert(identity.body.name === 'x', 'Lambda not working2');
assert(identity.isLambda, 'Lambda not working3');
assert(identity.isExp, 'Lambda not working3');

////////////////////////////////////////////////
const intVal12 = IntVal(12);
assert(intVal12.i === 12, 'IntVal not working1');
assert(intVal12.isIntVal, 'IntVal not working2');
assert(intVal12.isValue, 'IntVal not working3');

const one_var_env = Env({ w: IntVal(123) });

const funValIdentity = FunVal('x')(Var('x'))(one_var_env);
assert(funValIdentity.isFunVal, 'FunVal not working1');
assert(funValIdentity.isValue, 'FunVal not working2');
assert(funValIdentity.name === 'x', 'FunVal not working3');
assert(funValIdentity.exp.name === 'x', 'FunVal not working4');
assert(funValIdentity.env.lookup('w').get().i === 123, 'FunVal not working3');

////////////////////////////////////////////////
const eval0Var = eval0(one_var_env)(Var('w'));
assert(eval0Var.isIntVal, 'eval0 not working21');
assert(eval0Var.i === 123, 'eval0 not working11');

const eval0Lit = eval0(one_var_env)(Lit(234));
assert(eval0Lit.isIntVal, 'eval0 not working21');
assert(eval0Lit.i === 234, 'eval0 not working22');

const eval0WPlus123 = eval0(one_var_env)(Plus(Var('w'))(Lit(123)));
assert(eval0WPlus123.isIntVal, 'eval0 not working31');
assert(eval0WPlus123.i === 246, 'eval0 not working32');


if (typeof window !== 'undefined') {
  window.Env = Env;
  window.Lit = Lit;
  window.Var = Var;
  window.Plus = Plus;
  window.IntVal = IntVal;
}
