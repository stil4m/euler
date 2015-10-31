var _ = require('lodash');
var util = require('./util');


function euler1() {
  return _.sum(util.range(1, 999).filter(util.or(
    util.divides(3),
    util.divides(5)
  )));
}


console.log(euler1());