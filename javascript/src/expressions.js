
function Exp() {
  return {
    isExp: true,
  };
}

function Lit(i) {
  if (!Number.isInteger(i)) throw `Lit - not an integer: ${JSON.stringify(i)}`;
  let result = {
    isLit: true,
    i,
  };
  result = Object.setPrototypeOf(result, Exp());
  return result;
}

function Var(name = '') {
  if (typeof name !== 'string') throw `Var - not a string: ${JSON.stringify(name)}`;
  let result = {
    isVar: true,
    name,
  };
  result = Object.setPrototypeOf(result, Exp());
  return result;
}

export {
  Lit,
  Var,
};
