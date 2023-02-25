
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

function Plus(e1) {
  if (!e1.isExp) throw `Plus - e1 is not an Exp: ${JSON.stringify(e1)}`;
  return e2 => {
    if (!e2.isExp) throw `Plus - e2 is not an Exp: ${JSON.stringify(e2)}`;
    let result = {
      isPlus: true,
      e1,
      e2,
    };
    result = Object.setPrototypeOf(result, Exp());
    return result;
  }
}

function Lambda(argname) {
  if (typeof argname !== 'string') throw `Lambda - argname is not a string: ${JSON.stringify(argname)}`;
  return body => {
    if (!body.isExp) throw `Lambda - body is not an Exp: ${JSON.stringify(body)}`;
    let result = {
      isLambda: true,
      argname,
      body,
    };
    result = Object.setPrototypeOf(result, Exp());
    return result;
  }
}

export {
  Var,
  Lit,
  Plus,
  Lambda,
};
