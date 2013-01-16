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

    function define(form, env) {
        var value = evaluate(form.astnode, env);
        
        if( env.hasOwnBinding(form.symbol.value) ) {
            throw new SpecialFormError('ValueError', 'unbound symbol', 
                    'bound symbol ' + form.symbol.value, 'define', 'cannot redefine symbol');
        }
        
        env.addBinding(form.symbol.value, value);
        return Data.Null();
    }
    
    
    function set(form, env) {
        var value = evaluate(form.astnode, env);
        
        if( !env.hasBinding(form.symbol.value) ) {
            throw new SpecialFormError('ValueError', 'bound symbol', 
                    'unbound symbol ' + form.symbol.value, 'set', 'cannot set undefined symbol');
        }
        
        env.setBinding(form.symbol.value, value);
        return Data.Null();
    }
    
    
    function cond(form, env) {
        var pairs = form.pairs.elements,
            test, i;
        
        for(i = 0; i < pairs.length; i++) {
            test = evaluate(pairs[i].elements[0], env);
            typeCheck('boolean', test.type, 'cond', "condition of argument " + (i + 1));
            
            if(test.value) {
                return evaluate(pairs[i].elements[1], env);
            }
        }
        
        // if we didn't find a true condition
        return evaluate(form.elseValue, env);
    }
    
    
    // ASTList Symbol -> [Symbol]
    function extractArgNames(args) {
        return args.elements.map(function(sym) {
            return sym;
        });
    }
    
    
    function makeArray(size, initial) {
        var arr = [];
        for(var i = 0; i < size; i++) {
            arr.push(initial);
        }
        return arr;
    }


    function lambda(form, env) {
        var args = form.parameters,
            bodies = form.bodyForms,
            last   = form.lastForm,
            names = extractArgNames(args);

        // create the closure,
        //   which stores a reference to the environment,
        //   and when evaluated, creates a new environment
        //   and puts its arguments in the environment
        //   then it evaluates its body in the new environment
        function closure(c_args) {
            var newEnv = Environment.Environment(env, {}),
                j, k;

            // put parameter bindings into local environment
            for (j = 0; j < names.length; j++) {
                newEnv.addBinding(names[j].value, c_args[j]);
            }

            // evaluate all the body forms ...
            for(k = 0; k < bodies.length; k++) {
                evaluate(bodies[k], newEnv);
            }

            // ... and return the result of the last
            return evaluate(last, newEnv);
        }

        return Data.Function(makeArray(names.length, null), 'closure', closure);
    }


    ///////////

    function getDefaultEnv() {
        var bindings = {},
            name;
        
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


    function evaluateApplication(sexpr, env) {
        var first = evaluate(sexpr.operator, env),
            args = sexpr.arguments,
            optype = first.type;

        if (optype === 'function') {
            return applyFunction(first, env, args);
        }

        throw new Error("first element in Application must be function (was " + optype + ")");
    }
    
    
    function evaluateList(sexpr, env) {
        var elems = sexpr.elements.map(function(e) {
            return evaluate(e, env);
        });
        
        return Data.List(elems);
    }


    function evaluateSymbol(sexpr, env) {
        if(env.hasBinding(sexpr.value)) {
            return env.getBinding(sexpr.value);
        } else {
            throw new SpecialFormError('UndefinedVariableError', '', '', 'evaluateAtom', 'symbol ' + sexpr.value + ' is not defined');
        }
    }


    function evaluate(form, env) {
        env.logEvaluation(form);
        
        if (!env || !form) {
            throw new Error("cannot evaluate if missing form or environment");
        }
        
        var actions = {
                'application':  evaluateApplication,
                'list'       :  evaluateList,
                'define'     :  define,
                'set'       :  set,
                'cond'       :  cond,
                'lambda'     :  lambda,
                'char'       :  function(form, e) {return Data.Char(form.value);},
                'number'     :  function(form, e) {return Data.Number(form.value);},
                'symbol'     :  evaluateSymbol
            },
            action = actions[form.asttype];
        
        if(action) {
            return action(form, env);
        }

        throw new Error("unrecognized syntax type: " + form.asttype + " in " + JSON.stringify(form));
    }



    return {
        'eval'         :  evaluate,
        'getDefaultEnv':  getDefaultEnv,
        'define'       :  define,
        'set'         :  set,
        'lambda'       :  lambda,
        'cond'         :  cond
    };

})(Data, Functions, Environment);