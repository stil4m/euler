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
  },
  fibonacci: function(f) {
    function fib(x) {
      return [x[1], x[0] + x[1]];
    }

    var index = 0;
    var value = [1, 1];
    var answer = [1];
    while (true) {
      value = fib(value);
      if (!f(answer.length + 1, value[1])) {
        break;
      }
      answer.push(value[1]);
    }
    return answer;
  }

};
