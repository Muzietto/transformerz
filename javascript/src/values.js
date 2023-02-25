
function Value() {
  return {
    isValue: true,
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

export {
  IntVal,
};
