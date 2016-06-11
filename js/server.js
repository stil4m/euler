var express = require('express');
var app = express();

var problems = require('./g1');

app.get('/problems/:problem', function (req, res) {
  res.json(problems[req.params.problem]());
});

app.listen(3000, function () {
  console.log('Example app listening on port 3000!');
});
