
var Beagle = (function(parse, evaluate) {

var env = evaluate.getDefaultEnv();


function exec(str) {
  var parsed, prims, evaled;
  
  parsed = parse.parse(str);
  
  prims = evaluate.makePrimitives(parsed);
  
  evaled = evaluate.eval(prims, env);
  
  return {
	  'string': str,
	  'result': evaled,
	  'parsed': parsed,
	  'primitives': prims
  };
}


function execAll(str) {
  var result, prims, evaled;
  
  results = parse.parseAll(str);
  
  prims = results.sexprs.map(function(sexpr) {return evaluate.makePrimitives(sexpr);});
  
  evaled = prims.map(function(p) {return evaluate.eval(p, env)});
  
  return {
	  'string': str,
	  'result': evaled,
	  'parsed': result,
	  'primitives': prims
  };
	
}


return {
  'exec'         :  exec,
  'execAll'      :  execAll,
  'environment'  :  env
};

})(Parse, Evaluate);
