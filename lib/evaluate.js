"use strict";

var Data = require('./data'),
    Functions = require('./functions'),
    Environment = require('./environment');

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
    if( env.hasOwnBinding(form.symbol) ) {
        throw new SpecialFormError('ValueError', 'unbound symbol',
                'bound symbol ' + form.symbol, 'define', 'cannot redefine symbol');
    }
    
    var value = evaluate(form.value, env);
    
    env.addBinding(form.symbol, value);
    return Data.Null();
}


function set(form, env) {
    if( !env.hasBinding(form.symbol) ) {
        throw new SpecialFormError('ValueError', 'bound symbol',
                'unbound symbol ' + form.symbol, 'set', 'cannot set undefined symbol');
    }
    
    var value = evaluate(form.value, env);
    
    env.setBinding(form.symbol, value);
    return Data.Null();
}


function cond(form, env) {
    var pairs = form.branches,
        test, i;
    
    for(i = 0; i < pairs.length; i++) {
        test = evaluate(pairs[i][0], env);
        typeCheck('boolean', test.type, 'cond', "condition of argument " + (i + 1));
        
        if(test.value) {
            return evaluate(pairs[i][1], env);
        }
    }
    
    // if we didn't find a true condition
    return evaluate(form.elseValue, env);
}


function makeArray(size, initial) {
    var arr = [];
    for(var i = 0; i < size; i++) {
        arr.push(initial);
    }
    return arr;
}


function lambda(form, env) {
    var names = form.parameters,
        bodies = form.bodies,
        last   = form.returnValue;

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
            newEnv.addBinding(names[j], c_args[j]);
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
    for (name in Functions) {
        bindings[name] = Functions[name];
    }

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
            'application' :  evaluateApplication,
            'list'        :  evaluateList,
            'define'      :  define,
            'set'         :  set,
            'cond'        :  cond,
            'lambda'      :  lambda,
            'char'        :  function(form, e) {return Data.Char(form.value);},
            'number'      :  function(form, e) {return Data.Number(form.value);},
            'symbol'      :  evaluateSymbol
        },
        action = actions[form.asttype];
    
    if(action) {
        return action(form, env);
    }

    throw new Error("unrecognized syntax type: " + form.asttype + " in " + JSON.stringify(form));
}


module.exports = {
    'eval'         :  evaluate,
    'getDefaultEnv':  getDefaultEnv,
    'define'       :  define,
    'set'          :  set,
    'lambda'       :  lambda,
    'cond'         :  cond
};

