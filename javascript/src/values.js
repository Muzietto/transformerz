
function Value() {
  return {
    isValue: true,
    J: function() { return JSON.stringify(this); },
  };
}

function IntVal(i) {
  if (!Number.isInteger(i)) throw `IntVal - not an integer: ${JSON.stringify(i)}`;
  let result = {
    isIntVal: true,
    i,
  };
  result = Object.setPrototypeOf(result, Value());
  return result;
}

function FunVal(name) {
  if (typeof name !== 'string') throw `FunVal - name is not a string: ${JSON.stringify(name)}`;
  return exp => {
    if (!exp.isExp) throw `FunVal - exp is not an Exp: ${JSON.stringify(exp)}`;
    return env => {
      if (!env.isEnv) throw `FunVal - env is not an Env: ${JSON.stringify(env)}`;
      let result = {
        isFunVal: true,
        name,
        exp,
        env,
      };
      result = Object.setPrototypeOf(result, Value());
      return result;
    }
  }
}

export {
  IntVal,
  FunVal,
};
