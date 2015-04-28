"use strict";

var Environment = require('./environment'),
    Typebuilder = require('./typebuilder');


function log(obj) {
    console.log(require('util').inspect(obj, {depth: null}));
}

//////////////// a little prelude

function FunctionError(type, expected, actual, funcname, message) {
    this.type = type;
    this.expected = expected;
    this.actual = actual;
    this.funcname = funcname;
    this.message = message;
}

FunctionError.prototype.toString = function() {
    return this.type + " in " + this.funcname + ": " + this.message + ", expected " + this.expected + " but got " + this.actual;
};

function SpecialFormError(type, expected, actual, sfname, message) {
    this.type = type;
    this.expected = expected;
    this.actual = actual;
    this.sfname = sfname;
    this.message = message;
}

SpecialFormError.prototype.toString = function() {
    return this.type + " in " + this.sfname + ": " + this.message + ", expected " + this.expected + " but got " + this.actual;
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


//////////////// the data

var typeDefs = [
    ['Number',      ['value'             ]],
    ['Symbol',      ['value'             ]],
    ['String',      ['value'             ]],
    ['Boolean',     ['value'             ]],
    ['List',        ['elems'             ]],
    ['Nil',         [                    ]],
//    ['Dictionary',  ['keyvals'              ]],
    ['UserFunc',    ['name', 'params', 'forms', 'env']],
    ['BuiltinFunc', ['name', 'types', 'function']],
];

var typesAndCons = Typebuilder.makeTypes(typeDefs, 'datatype'),
    rootDataNode = typesAndCons.rootNode,
    types = typesAndCons.types,
    constructors = typesAndCons.constructors;

var beagleTrue = constructors.Boolean(true),
    beagleFalse = constructors.Boolean(false),
    beagleNil = constructors.Nil();


/////////////////// the functions

var functions = (function() {

    var cons = constructors.BuiltinFunc(
        'cons',
        [null, 'List'],
        function (args) {
            var elem = args[0],
                list = args[1],
                newList = [elem],
                i;
            for (i = 0; i < list.elems.length; i++) {
                newList.push(list.elems[i]);
            }
            return constructors.List(newList);
        }
    );

    var car = constructors.BuiltinFunc(
        'car',
        ['List'],
        function (args) {
            var list = args[0];
            if (list.elems.length === 0) {
                throw FunctionError("ValueError", "non-empty list", "empty list", 'car', "1st arg");
            }
            return list.elems[0];
        }
    );

    var cdr = constructors.BuiltinFunc(
        'cdr',
        ['List'],
        function (args) {
            var list = args[0];
            if (list.elems.length === 0) {
                throw FunctionError("ValueError", "non-empty list", "empty list", 'cdr', '1st arg');
            }
            return constructors.List(list.elems.slice(1));
        }
    );

    var nullQ = constructors.BuiltinFunc(
        'null?',
        ['List'],
        function (args) {
            var list = args[0];
            return (list.elems.length === 0) ? beagleTrue : beagleFalse;
        }
    );

    var plus = constructors.BuiltinFunc(
        '+',
        ['Number', 'Number'],
        function(args) {
            return constructors.Number(args[0].value + args[1].value);
        }
    );

    var COMPARABLE = {
        'number' : 1,
        'char'   : 1,
        'boolean': 1,
        'nil'    : 1,
    };

    var isEqual = constructors.BuiltinFunc(
        '=',
        [null, null],
        function(args) {
            var left = args[0],
                right = args[1],
                ltype = left.type,
                rtype = right.type;
            if (ltype !== rtype || !COMPARABLE.hasOwnProperty(ltype)) {
                return beagleFalse;
            }
            // TODO is `value` okay to use?  what about for nil?
            return (args[0].value === args[1].value) ? beagleTrue : beagleFalse;
        }
    );

    return {
        'cons'      :  cons,
        'car'       :  car,
        'cdr'       :  cdr,
        'null?'     :  nullQ,
        '+'         :  plus,
        '='         :  isEqual,
    };
})();


///////////

function getDefaultEnv() {
    var bindings = {
        // TODO empty list?
        'true'   : beagleTrue,
        'false'  : beagleFalse,
        'nil'    : beagleNil,
    };
    
    Object.getOwnPropertyNames(functions).forEach(function(name) {
        bindings[name] = functions[name];
    });

    return new Environment.Environment(null, bindings);
}

//// evaluation

function evalNumber(node, env) {
    return constructors.Number(node.numValue);
}

function evalSymbol(node, env) {
    if (env.hasBinding(node.strValue)) {
        return env.getBinding(node.strValue);
    } else {
        throw new SpecialFormError('UndefinedVariableError', '', '', 'evaluateAtom', 'symbol ' + node.strValue + ' is not defined');
    }
}

function evalString(node, env) {
    return constructors.String(node.strValue);
}

function evalList(node, env) {
    var elems = node.elems.map(function(e) {
        return evaluate(e, env);
    });
    return constructors.List(elems);
}

// TODO dictionary

function evalApplication(node, env) {
    var first = evaluate(node.operator, env),
        args = node.arguments;
    return applyFunction(first, env, args);
}

function applyFunction(func, env, args) {
    // TODO can this be cleaned up?
    var fname = (func.datatype === 'BuiltinFunc') ? func.name : 'user-defined function',
        expectedNumArgs = (func.datatype === 'BuiltinFunc') ? func.types.length : func.params.length;
    if (args.length !== expectedNumArgs) {
        throw new FunctionError("NumArgsError", func.types.length, evaledArgs.length, fname, '');
    }
    // `func` has already been evaluated
    var evaledArgs = args.map(function (arg) {
        return evaluate(arg, env);
    });

    if (func.datatype === 'UserFunc') {
        var newEnv = new Environment.Environment(env, {}),
            retVal = beagleNil;
        // put parameter bindings into local environment
        for (var j = 0; j < func.params.length; j++) {
            newEnv.addBinding(func.params[j], evaledArgs[j]);
        }
        // evaluate all the body forms
        for(var k = 0; k < func.forms.length; k++) {
            retVal = evaluate(func.forms[k], newEnv);
        }
        // and return the last form
        return retVal;
    } else if (func.datatype === 'BuiltinFunc') {
        for (var i = 0; i < evaledArgs.length; i++) {
            if (func.types[i] && (evaledArgs[i].datatype !== func.types[i])) {
                throw new FunctionError("TypeError", func.types[i], evaledArgs[i].datatype, func.name, ' arg ' + (i + 1));
            }
        }
        return func.function(evaledArgs);
    }
    
    throw new Error('invalid node type in applyFunction -- ' + func.datatype);
}

function evalDefine(node, env) {
    var name = node.symbol.strValue;
    if (env.hasOwnBinding(name)) {
        throw new SpecialFormError('ValueError', 'unbound symbol',
                'bound symbol ' + name, 'define', 'cannot redefine symbol');
    }
    var value = evaluate(node.value, env);
    env.addBinding(name, value);
    return beagleNil;
}

function evalSet(node, env) {
    var name = node.symbol.strValue;
    if (!env.hasBinding(name)) {
        throw new SpecialFormError('ValueError', 'bound symbol',
                'unbound symbol ' + name, 'set', 'cannot set undefined symbol');
    }
    var value = evaluate(node.value, env);
    env.setBinding(name, value);
    return beagleNil;
}

function evalCond(node, env) {
    var pairs = node.branches,
        test, i;
    
    for (i = 0; i < pairs.length; i++) {
        test = evaluate(pairs[i][0], env);
        typeCheck('Boolean', test.datatype, 'cond', "condition of argument " + (i + 1));
        
        if (test.value) {
            return evaluate(pairs[i][1], env);
        }
    }
    
    // didn't find a true condition
    return evaluate(node.elseValue, env);
}

function evalFn(node, env) {
    // TODO could put the position into the name
    var params = node.params.map(function(p) {return p.strValue;});
    return constructors.UserFunc('user function', params, node.forms, env);
}

function evalBeagle(node, env) {
    var evaled = [];
    // using forEach to emphasize the side effects from passing `env`
    node.forms.forEach(function(form) {
        evaled.push(evaluate(form, env));
    });
    return evaled;
}

var actions = {
        'Number'     : evalNumber,
        'Symbol'     : evalSymbol,
        'String'     : evalString,
        'List'       : evalList,
//            'Dictionary'  : evalDictionary, // TODO
        'Application': evalApplication,
        'Def'        : evalDefine,
        'Set'        : evalSet,
        'Cond'       : evalCond,
        'Fn'         : evalFn,
        'Beagle'     : evalBeagle,
    };

function evaluate(node, env) {
    if (!env || !node) {
        throw new Error("cannot evaluate: missing node or environment");
    }
    
    var action = actions[node.asttype];
    
    if (!action) {
        throw new Error("unrecognized syntax type: " + node.asttype + " in " + JSON.stringify(node));
    }

    return action(node, env);
}


module.exports = {
    'evaluate'     :  evaluate      ,
    'getDefaultEnv':  getDefaultEnv ,
    'actions'      :  actions       ,
    
    'rootDataNode' :  rootDataNode  ,
    'types'        :  types         ,
    'constructors' :  constructors  ,
};

