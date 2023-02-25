import { expect } from 'chai';
import cippo from '@src/index';

describe('in the magical world of index', () => {
  it('lives the cippo method', () => {
    expect(cippo(1, 2)).to.eql(5);
  });
});
