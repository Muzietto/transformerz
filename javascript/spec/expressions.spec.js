import { expect } from 'chai';
import {
  Lit,
  Var,
  Plus,
  Lambda,
} from '@src/expressions';

describe('in the magical world of expressions', () => {
  describe('lives the Lit operator that', () => {
    it('expresses literal integers', () => {
      expect(Lit(12)).to.eql(Lit(12));
      expect(Lit(12).i).to.eql(12);
      expect(Lit(12).isLit).to.be.true;
      expect(Lit(12).isExp).to.be.true;
    });
  });
  describe('lives the Var operator that', () => {
    it('expresses name of variables', () => {
      expect(Var('xxxx')).to.eql(Var('xxxx'));
      expect(Var('xxxx').name).to.eql('xxxx');
      expect(Var('xxxx').isVar).to.be.true;
      expect(Var('xxxx').isExp).to.be.true;
    });
  });
  describe('lives the Plus operator that', () => {
    it('expresses the sum of expressions', () => {
      const xPlusY = Plus(Lit(1))(Lit(2));
      expect(xPlusY.e1.i).to.eql(1);
      expect(xPlusY.e2.i).to.eql(2);
      expect(xPlusY.isPlus).to.be.true;
      expect(xPlusY.isExp).to.be.true;
    });
  });
  describe('lives the Lambda operator that', () => {
    it('expresses unary FUNCTIONS', () => {
      const identity = Lambda('x')(Var('x'));
      expect(identity.argname).to.eql('x');
      expect(identity.body.isVar).to.be.true;
      expect(identity.body.name).to.eql('x');
      expect(identity.isLambda).to.be.true;
      expect(identity.isExp).to.be.true;
    });
  });
});
