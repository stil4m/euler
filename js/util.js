module.exports = {
  range: function (start, end) {
    var result = [];
    for (var i = 0; i <= end; i++) {
      result.push(i);
    }
    return result;
  },
  divides: function (x) {
    return function (y) {
      return y % x == 0;
    }
  },
  or: function () {
    var args = arguments;
    var props = Object.keys(arguments).map(function (k) {
      return args[k];
    });

    return function (x) {
      return props.filter(function (prop) {
          return prop(x)
        }).length > 0;
    };
  }
};