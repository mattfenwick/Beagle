var Evaluate = (function (Data, Functions, Environment) {
    "use strict";


    function SpecialFormError(type, expected, actual, sfname, message) {
        this.type = type;
        this.expected = expected;
        this.actual = actual;
        this.sfname = sfname;
        this.message = message;
    }


    SpecialFormError.prototype.toString = function() {
        return this.type + " in " + this.sfname + ": " + this.message + 
               ", expected " + this.expected + " but got " + this.actual;
    };


    function typeCheck(expected, actual, fname, message) {
        if (expected !== actual) {
            throw new SpecialFormError('TypeError', expected, actual, fname, message);
        }
    }


    function argsCheck(expected, actual, fname, message) {
        if (expected !== actual) {
            throw new SpecialFormError('NumArgsError', expected, actual, fname, message);
        }
    }
    

    //////// Special forms

    function define(env, args) {
        argsCheck(2, args.length, 'define');

        var name = args[0], 
            sexpr = args[1],
            value;
        
        typeCheck('symbol', name.type, 'define', 'first argument');
        
        value = evaluate(sexpr, env);
        
        if( env.hasOwnBinding(name.value) ) {
            throw new SpecialFormError('ValueError', 'unbound symbol', 
                    'bound symbol ' + name.value, 'define', 'cannot redefine symbol');
        }
        
        env.addBinding(name.value, value);
        return Data.Null();
    }
    
    
    function setBang(env, args) {
        argsCheck(2, args.length, 'set!');
        
        var name = args[0],
            sexpr = args[1],
            value;
        
        typeCheck('symbol', name.type, 'set!', 'first argument');
        
        value = evaluate(sexpr, env);
        
        if( !env.hasBinding(name.value) ) {
            throw new SpecialFormError('ValueError', 'bound symbol', 
                    'unbound symbol ' + name.value, 'set!', 'cannot set! undefined symbol');
        }
        
        env.setBinding(name.value, value);
        return Data.Null();
    }


    function myif(env, args) {
        argsCheck(3, args.length, 'if');
        
        var condition = args[0], 
            ifTrue = args[1],
            ifFalse = args[2],
            cond = evaluate(condition, env);

        typeCheck('boolean', cond.type, 'if', "first argument");

        if (cond.value) {
            return evaluate(ifTrue, env);
        }

        return evaluate(ifFalse, env);
    }
    
    
    function cond(env, args) {
        var i,
            test;
        
        for(i = 0; i < args.length; i++) {
            typeCheck('list', args[i].type, 'cond', "argument " + (i + 1));
            argsCheck(2, args[i].value.length, 'arguments to cond must be lists of length 2');
            
            test = evaluate(args[i].value[0], env);
            typeCheck('boolean', test.type, 'cond', "condition of argument " + (i + 1));
            
            if(test.value) {
                return evaluate(args[i].value[1], env);
            }
        }
        
        throw new SpecialFormError('ValueError', 'a true condition',
                  'no true condition', 'cond', "a true condition is required");
    }
    
    
    function extractArgNames(args) {
        var names = [],
            i = 1;

        args.map(function(sym) {
            typeCheck('symbol', sym.type, 'lambda/special parameters', "parameter " + i);
            i++;

            names.push(sym);
        });
        
        return names;
    }
    
    
    function makeArray(size, initial) {
        var arr = [];
        for(var i = 0; i < size; i++) {
            arr.push(initial);
        }
        return arr;
    }


    function lambda(env, lam_args) {
        if( lam_args.length < 2 ) {
            throw new SpecialFormError("NumArgsError", "list of arguments and at least 1 body form",
                      "missing one or both", "lambda/special", "body may not be empty");
        }
        
        typeCheck('application', lam_args[0].type, 'lambda/special', 'first argument');
        
        var args = lam_args[0].getAllArgs(), // get args out of the unused 'Application'
            bodies = lam_args.slice(1),
            names = extractArgNames(args);

        // create the closure,
        //   which stores a reference to the environment,
        //   and when evaluated, creates a new environment
        //   and puts its arguments in the environment
        //   then it evaluates its body in the new environment
        function closure(c_args) {
            var newEnv = Environment.Environment(env, {}),
                j, k;

            for (j = 0; j < names.length; j++) {
                // arguments don't need to be evaluated here
                newEnv.addBinding(names[j].value, c_args[j]);
            }

            // evaluate all the body forms except for the last ...
            for(k = 0; k < bodies.length - 1; k++) {
                evaluate(bodies[k], newEnv);
            }

            // ... because we want to return its result
            return evaluate(bodies[k], newEnv);
        }

        return Data.Function(makeArray(names.length, null), 'closure', closure);
    }
    
    
    function quote(env, args) {
        argsCheck(1, args.length, 'quote');

        return args[0];
    }

    
    /////////// core functions
    
    function beagleEval(env, args) {
        argsCheck(1, args.length, 'eval');
        
        var sexpr = evaluate(args[0], env);

        return evaluate(sexpr, env);
    }


    ///////////

    function getDefaultEnv() {
        var bindings = {},
            name;

        // the special forms
        bindings['define'] = Data.SpecialForm(define);
        bindings['set!']   = Data.SpecialForm(setBang);
        bindings['if']     = Data.SpecialForm(myif);
        bindings['cond']   = Data.SpecialForm(cond);
        bindings['lambda'] = Data.SpecialForm(lambda);
        bindings['quote']  = Data.SpecialForm(quote);

        // the special form that should be a function
        bindings['eval']   = Data.SpecialForm(beagleEval);
        
        // the boolean constants
        bindings['true']   = Data.Boolean(true);
        bindings['false']  = Data.Boolean(false);

        // the functions
        for(name in Functions) {
            bindings[name] = Functions[name];
        };

        return Environment.Environment(null, bindings);
    }

    ////////////
    
    
    function applyFunction(func, env, args) {
        var evaledArgs = args.map(function (arg) {
            return evaluate(arg, env);
        });

        return func.fapply(evaledArgs);
    }
    
    
    function applySpecial(func, env, args) {
        return func.value(env, args);
    }


    function evaluateApplication(sexpr, env) {
        var first = evaluate(sexpr.getOperator(), env),
            args = sexpr.getArgs();

        if (first.type === 'function') {
            return applyFunction(first, env, args);
        }

        if (first.type === 'specialform') {
            return applySpecial(first, env, args);
        }

        throw new Error("first element in list must be function or special form (was " + first.type + ")");
    }


    var SELF_EVALUATING_TYPES  = {
        'number'       :  1,
        'char'         :  1,
        'boolean'      :  1,
        'function'     :  1,
        'specialform'  :  1,
        'list'         :  1
    };


    function evaluateAtom(sexpr, env) {
        var type = sexpr.type;
        
        if (type === 'symbol') {
            if(env.hasBinding(sexpr.value)) {
                return env.getBinding(sexpr.value);
            } else {
                throw new SpecialFormError('UndefinedVariableError', '', '', 'evaluateAtom', 'symbol ' + sexpr.value + ' is not defined');
            }
        }

        if (type in SELF_EVALUATING_TYPES) {
            return sexpr;
        }

        throw new Error("unrecognized type: " + type + " in " + JSON.stringify(sexpr));
    }


    function evaluate(sexpr, env) {
        env.logEvaluation(sexpr);
        
        if (!env || !sexpr) {
            throw new Error("evaluate missing sexpr or environment");
        }

        if (sexpr.type === 'application') {
            return evaluateApplication(sexpr, env);
        }

        return evaluateAtom(sexpr, env);
    }



    return {
        'eval'         :  evaluate,
        'getDefaultEnv':  getDefaultEnv,
        'define'       :  define,
        'set!'         :  setBang,
        'if'           :  myif,
        'lambda'       :  lambda,
        'quote'        :  quote,
        'beagleEval'   :  beagleEval,
        'cond'         :  cond
    };

})(Data, Functions, Environment);