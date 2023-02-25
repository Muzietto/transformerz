import { expect } from 'chai';
import {
  Env,
} from '@src/functions';

describe('in the magical world of functions', () => {
  describe('lives the Env environment that', () => {
    it('looks up keys', () => {
      expect(Env({ w: 123 }).lookup('w').get()).to.eql(123);
    });
    it('inserts keys and values', () => {
      expect(Env().insert('a')(false).lookup('a').get()).to.eql(false);
    });
  });
});
