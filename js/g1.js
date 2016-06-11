var _ = require('lodash');
var util = require('./util');
var Primes = require('./primes').Primes;
var primeUtils = require('./primes').utils;

function euler1() {
  return util.range(1, 999)
    .filter(
      util.or(
        util.divides(3),
        util.divides(5)
      ))
    .reduce((x, y) => x + y, 0);
}

function euler2() {
  return util.fibonacci((_, v) => v < 4000000)
    .filter(x => x % 2 == 0)
    .reduce((x, y) => x + y, 0)
}

function euler3() {
  return _.last(primeUtils.factors(600851475143));
}

function euler4() {
  
}

module.exports = {
  1: euler1,
  2: euler2,
  3: euler3
}
