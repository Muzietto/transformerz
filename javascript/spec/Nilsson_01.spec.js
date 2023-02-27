import { expect } from 'chai';
import {
  Env,
} from '@src/functions';
import {
  Var,
  Lit,
  Plus,
  Lambda,
  App,
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
  const lambdaXPlusY = Lambda('x')(Lambda('y')(Plus(Var('x'))(Var('y'))));

  // 12 + (\x -> x)(4 + 2)
  const sample = Plus(Lit(12))(App(identity)(Plus(Lit(4))(Lit(2)))); // -- IntVal 18
  //const samplone = App(App(lambdaXPlusY)(Lit(4)))(Var('xxxx')) // -- IntVal (4 + xxxx)

  describe('lives eval0 that', () => {
    it('should evaluate a literal', () => {
      const intVal123 = eval0(two_vars_env)(Lit(123))
      expect(intVal123.J()).to.equal(IntVal(123).J());
    });
    it('should lookup a var', () => {
      const lookedUpXxxx = eval0(two_vars_env)(watIsXxxx);
      expect(lookedUpXxxx.J()).to.equal(IntVal(123).J());
    });
    it('should sum two vars', () => {
      const eval0XPlusY = eval0(two_vars_env)(xPlusY);
      expect(eval0XPlusY.J()).to.equal(IntVal(357).J());
    });
    it('should eval a function given an environment', () => {
      const evaluedIdentity = eval0(two_vars_env)(identity);
      expect(evaluedIdentity.J()).to.be.equal(FunVal('x')(Var('x'))(two_vars_env).J());
    });
    it('should make a few VERY simple applications - 1', () => {
      expect(eval0(two_vars_env)(App(identity)(watIsXxxx)).J()).to.equal(IntVal(123).J());
      const one = eval0(two_vars_env)(App(lambdaXPlusY)(watIsXxxx));
      const two = FunVal('y')(Plus(Var('x'))(Var('y')))(two_vars_env.insert('x')(IntVal(123)));
      expect(eval0(two_vars_env)(App(lambdaXPlusY)(watIsXxxx)).J())
        .to.equal(FunVal('y')(Plus(Var('x'))(Var('y')))(two_vars_env.insert('x')(IntVal(123))).J());
    });
    xit('should make a few VERY simple applications - 2', () => {
      expect(eval0(two_vars_env)(App(App(lambdaXPlusY)(watIsXxxx))(watIsYyyy)).J())
        .to.equal(IntVal(357).J());
    });
    it('should make a simple application', () => {
      expect(eval0(Env({}))(sample).J()).to.equal(IntVal(18).J());
    });
  });
});
