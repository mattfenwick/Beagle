
var Beagle = (function(parse, evaluate) {

function lisp(str) {
  var parsed, prims, evaled;
  parsed = parse.parse(str);
  prims = evaluate.makePrimitives(parsed);
  evaled = evaluate.eval(prims);
  return evaled;
}

return {
  'lisp' : lisp,
};

})(Parse, Evaluate);
