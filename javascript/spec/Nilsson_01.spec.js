import { expect } from 'chai';
import {
  Env,
} from '@src/functions';

/*
xdescribe('in the magical world of Nilsson_01', () => {
  const watIsXxxx = Var('xxxx');
  const watIsYyyy = Var('yyyy');

  const two_vars_env = Env({
    xxxx: IntVal(123),
    yyyy: IntVal(234),
  });
  const xPlusY = Plus(Var('xxxx'))(Var('yyyy'));
  // \x -> x
  const lambdina = Lambda('x')(Var('x')); // IDENTITY
  // \x -> \y -> x + y
  const lambdona = Lambda('x')(Lambda('y')(Plus(Var('x'))(Var('y'))));
  // 12 + (\x -> x)(4 + 2)
  const sample = Plus(Lit(12))(App(lambdina)(Plus(Lit(4))(Lit(2)))); // -- IntVal 18
  const samplone = App(App(lambdona)(Lit(4)))(Var('xxxx')) // -- IntVal (4 + xxxx)

  describe('lives eval0 that', () => {
    it('should lookup a var', () => {
      expect(eval0(two_vars_env)(watIsXxxx)).to.eql(IntVal(123));
    });
    it('should sum two vars', () => {
      expect(eval0(two_vars_env)(xPlusY)).to.eql(IntVal(357));
    });
    it('should make a VERY simple application', () => {
      expect(eval0(two_vars_env)(App(watIsXxxx)(lambdina))).to.eql(IntVal(123));
    });
    it('should make a simple application', () => {
      expect(eval0(Env({}))(sample)).to.eql(IntVal(127));
    });
  });
});
*/
