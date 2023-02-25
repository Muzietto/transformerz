import { expect } from 'chai';
import {
  Lit,
  Var,
} from '@src/expressions';

describe('in the magical world of expressions', () => {
  describe('lives the Lit operator that', () => {
    it('expresses literal integers', () => {
      expect(Lit(12).i).to.eql(12);
      expect(Lit(12).isLit).to.be.true;
      expect(Lit(12).isExp).to.be.true;
    });
  });
  describe('lives the Var operator that', () => {
    it('expresses name of variables', () => {
      expect(Var('xxxx').name).to.eql('xxxx');
      expect(Var('xxxx').isVar).to.be.true;
      expect(Var('xxxx').isExp).to.be.true;
    });
  });
});
