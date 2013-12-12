define([
  'app/beagle', 
  'app/evaluate', 
  'unparse-js/maybeerror'
], function(beagle, evaluate, me) {
    "use strict";

    function Model(env) {
        this.environment = env;
    }
    
    Model.prototype.evalString = function(str) {
        var asts = beagle.parse(str),
            results = [];
        if ( asts.status !== 'success' ) {
            return asts;
        }
        try {
            for(var i = 0; i < asts.value.length; i++) {
                results.push(evaluate.eval(asts.value[i], this.environment));
            }
            return me.pure(results);
        } catch(e) {
            return me.error({'cause': 'evaluation', 'results': results, 'error': e});
        }
    };
    
    return Model;
    
});