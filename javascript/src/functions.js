
function Env(dict = {}) {
  const parent = {
    isEnv: true,
    lookup: function(key) {
      const result = this[key];
      return result;
    },
    insert: function(key) {
      return value => {
        return Env({
          ...this,
          [key]: value,
        });
      }
    }
  };
  let result = {
    ...dict,
  };
  result = Object.setPrototypeOf(result, parent);
  return result;
}

export {
  Env,
};
