var Beagle = (function(parse, evaluate) {
"use strict";

var env = evaluate.getDefaultEnv();


function primMaker(sexpr) {
  return evaluate.makePrimitives(sexpr);
}


function evaler(p) {
  return evaluate.eval(p, env);
}


// String -> Maybe [SExpression]
//   returns false if, after all s-expressions are extracted,
//   there are still tokens left in the token stream
function parseString(str) {
  var allTokens = parse.tokenize(str),
      tokens = parse.stripTokens(allTokens),
      sexprs = parse.makeSExpressions(tokens);

  return sexprs;
}


function exec(str) {
  var results = parseString(str),
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
  'environment'  :  env,
  'parseString'  :  parseString
};

})(Parse, Evaluate);
