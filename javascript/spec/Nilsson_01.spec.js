import { expect } from 'chai';
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
} from '@src/Nilsson_01';

describe('in the magical world of Nilsson_01', () => {
  const watIsXxxx = Var('xxxx');
  const watIsYyyy = Var('yyyy');

  const two_vars_env = Env({
    xxxx: IntVal(123),
    yyyy: IntVal(234),
  });
  const xPlusY = Plus(Var('xxxx'))(Var('yyyy'));
  // \x -> x
  const identity = Lambda('x')(Var('x')); // IDENTITY
  // \x -> \y -> x + y
  //const lambdona = Lambda('x')(Lambda('y')(Plus(Var('x'))(Var('y'))));
  // 12 + (\x -> x)(4 + 2)
  //const sample = Plus(Lit(12))(App(lambdina)(Plus(Lit(4))(Lit(2)))); // -- IntVal 18
  //const samplone = App(App(lambdona)(Lit(4)))(Var('xxxx')) // -- IntVal (4 + xxxx)

  describe('lives eval0 that', () => {
    it('should evaluate a literal', () => {
      const intVal123 = eval0(two_vars_env)(Lit(123))
      expect(intVal123).to.eql(IntVal(123));
    });
    it('should lookup a var', () => {
      const lookedUpXxxx = eval0(two_vars_env)(watIsXxxx);
      expect(lookedUpXxxx).to.eql(IntVal(123));
    });
    it('should sum two vars', () => {
      const eval0XPlusY = eval0(two_vars_env)(xPlusY);
      expect(eval0XPlusY).to.eql(IntVal(357));
    });
    it('should eval a function given an environment', () => {
      const evaluedIdentity = eval0(two_vars_env)(identity);
      expect(evaluedIdentity).to.be.eql(FunVal('x')(Var('x'))(two_vars_env));
    });
    xit('should make a VERY simple application', () => {
      expect(eval0(two_vars_env)(App(watIsXxxx)(lambdina))).to.eql(IntVal(123));
    });
    xit('should make a simple application', () => {
      expect(eval0(Env({}))(sample)).to.eql(IntVal(127));
    });
  });
});
