var Primes = function() {

  var list = [];

  function isPrime(n) {
    var root = Math.sqrt(n);

    for (var i = 0; i < list.length; i++) {
      if (list[i] > root) {
        return true;
      }
      if (n % list[i] == 0) {
        return false;
      }
    }
    return true;
  }

  return {
    next: function() {
      if (list.length === 0) {
        list.push(2);
        return 2;
      }
      if (list.length === 1) {
        list.push(3);
        return 3;
      }

      var next = list[list.length - 1];
      do {
        next += 2;
      } while (!isPrime(next));

      list.push(next);
      return next;
    }
  }
}
module.exports = {
  Primes: Primes,
  utils: {
    factors: function(x) {
      var primes = new Primes();
      var target = x;
      var answer = [];
      while (true) {
        var nextPrime = primes.next();
        while (target % nextPrime == 0) {
          target = target / nextPrime;
          answer.push(nextPrime);
        }
        if (target == 1) {
          return answer;
        }
      }
    }
  }
}
