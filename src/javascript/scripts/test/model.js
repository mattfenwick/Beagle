define([
  'app/evaluate', 
  'repl/model', 
  'app/data', 
  'unparse-js/maybeerror'
], function(Evaluate, Model, data, me) {

    module("model");
    
    return function () {

        var model = new Model(Evaluate.getDefaultEnv());
        
        test("execution error", function() {
            var r = model.evalString('1 x');
            deepEqual('error', r.status);
            deepEqual('evaluation', r.value.cause);
        });
        
        test("successful execution", function() {
            var r = model.evalString('(+ 3 2); qrs \n "ab"');
            deepEqual(me.pure([data.Number(5), data.makeCharList('ab')]), r);
        });
    
    };
    
});