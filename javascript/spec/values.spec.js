import { expect } from 'chai';
import {
  IntVal,
  FunVal,
} from '@src/values';
import {
  Var,
  Lit,
  Plus,
  Lambda,
} from '@src/expressions';
import {
  Env,
} from '@src/functions';

describe('in the magical world of values', () => {
  describe('lives the IntVal operator that', () => {
    it('expresses the VALUE of integers', () => {
      expect(IntVal(12).i).to.eql(12);
      expect(IntVal(12).isIntVal).to.be.true;
      expect(IntVal(12).isValue).to.be.true;
    });
  });
  describe('lives the FunVal operator that', () => {
    const one_var_env = Env({ w: IntVal(123) });
    it('expresses the VALUE of a function inside a given environment', () => {
      const funValIdentity = FunVal('x')(Var('x'))(one_var_env);
      expect(funValIdentity.isFunVal).to.be.true;
      expect(funValIdentity.isValue).to.be.true;
      expect(funValIdentity.name).to.be.eql('x');
      expect(funValIdentity.exp.name).to.be.eql('x');
      expect(funValIdentity.env.lookup('w').get().i).to.be.eql(123);
    });
  });
});
