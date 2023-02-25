import { expect } from 'chai';
import {
  IntVal,
} from '@src/values';

describe('in the magical world of values', () => {
  describe('lives the IntValue operator that', () => {
    it('expresses the VALUE of integers', () => {
      expect(IntVal(12).i).to.eql(12);
      expect(IntVal(12).isIntVal).to.be.true;
      expect(IntVal(12).isValue).to.be.true;
    });
  });
});
