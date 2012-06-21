
var Beagle = (function(parse, evaluate) {

var env = evaluate.getDefaultEnv();


function primMaker(sexpr) {
  return evaluate.makePrimitives(sexpr);
}


function evaler(p) {
  return evaluate.eval(p, env);
}


function exec(str) {
  var results = parse.parse(str),
      prims = results.map(primMaker),
      evaled = prims.map(evaler);
  
  return {
    'string': str,
    'result': evaled,
    'parsed': results,
    'primitives': prims
  };
}


return {
  'exec'         :  exec,
  'environment'  :  env
};

})(Parse, Evaluate);
