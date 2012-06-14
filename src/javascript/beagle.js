
var Beagle = (function(parse, evaluate) {

var env = evaluate.getDefaultEnv(),
    lastParse = null,
    lastPrims;


function lisp(str) {
  var evaled;
  lastParse = parse.parse(str);
  lastPrims = evaluate.makePrimitives(lastParse);
  evaled = evaluate.eval(lastPrims, env);
  return evaled;
}


function getLastParse() {
  return lastParse;
}


function getLastPrims() {
  return lastPrims;
}


return {
  'lisp' : lisp,
  'environment': env,
  'getLastParse': getLastParse,
  'getLastPrims': getLastPrims
};

})(Parse, Evaluate);
