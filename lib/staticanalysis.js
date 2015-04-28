'use strict';

var environment = require('./environment');


var variableUsage = (function() {

    function symbol(node, state, env, log) {
        var name = node.strValue;
        if (!env.hasBinding(name)) {
            log.errors.push({'name': name, 'position': node._debug.start, 'message': 'using undefined variable'});
        } else {
            var binding = env.getBinding(name);
            binding.count++;
        }
    }

    function list(node, state, env, log) {
        node.elems.forEach(function(e) {
            variablesHelp(e, state, env, log);
        });
    }

    function application(node, state, env, log) {
        var forms = [node.operator].concat(node.arguments);
        forms.forEach(function(f) {
            variablesHelp(f, state, env, log);
        });
    }

    function def(node, state, env, log) {
        var name = node.symbol.strValue;
        if (env.hasOwnBinding(name)) {
            log.errors.push({'name': name, 'position': node._debug.start, 'message': 'duplicate variable definition'});
        } else if (env.hasBinding(name)) {
            log.warnings.push({'name': name, 'position': node._debug.start, 'message': 'variable shadowing'});
        } else {
            env.addBinding(name, {'position': node._debug.start, 'count': 0});
        }
        // notice how this doesn't recur on node.symbol
        variablesHelp(node.value, state, env, log);
    }

    function set(node, state, env, log) {
        if (!env.hasBinding(node.symbol.strValue)) {
            log.errors.push({'name': node.symbol.strValue, 'position': node._debug.start, 'message': 'setting undefined variable'});
        }
        // we're not going to count 'set' as using a variable
        variablesHelp(node.value, state, env, log);
    }

    function cond(node, state, env, log) {
        node.branches.forEach(function(b) {
            variablesHelp(b[0], state, env, log);
            variablesHelp(b[1], state, env, log);
        });
        variablesHelp(node.elseValue, state, env, log);
    }

    function fn(node, state, env, log) {
        // push a new env on, add bindings for each of its variables
        var newEnv = new environment.Environment(env, {});
        node.params.forEach(function(p) {
            if (env.hasBinding(p.strValue)) {
                log.warnings.push({'name': p.strValue, 'position': p._debug.start, 'message': 'variable shadowing'});
            }
            newEnv.addBinding(p.strValue, {'position': p._debug.start, 'count': 0});
        });
        // go through its children
        node.forms.forEach(function(f) {
            variablesHelp(f, state, newEnv, log);
        });
        // now examine the scope's variables, see if they were all used
        Object.getOwnPropertyNames(newEnv.bindings).forEach(function(prop) {
            var binding = newEnv.getBinding(prop);
            console.log('checking ' + prop + " " + JSON.stringify(binding));
            if (binding.count === 0) {
                var pos = binding.position;
                log.warnings.push({'name': prop, 'position': pos, 'message': 'unused variable'});
            }
        });
    }

    function beagle(node, state, env, log) {
        node.forms.forEach(function(f) {
            variablesHelp(f, state, env, log);
        });
        // examine the scope's variables, see if they were all used
        Object.getOwnPropertyNames(env.bindings).forEach(function(prop) {
            var binding = env.getBinding(prop);
            if (binding.count === 0) {
                var pos = binding.position;
                log.warnings.push({'name': prop, 'position': pos, 'message': 'unused variable'});
            }
        });
    }

    var actions = {
        'Number': function() {},
        'Symbol': symbol,
        'String': function() {},
        'List'  : list,
        // TODO dictionary
        'Application': application,
        'Def'   : def,
        'Set'   : set,
        'Cond'  : cond,
        'Fn'    : fn,
        'Beagle': beagle,
    };

    function variablesHelp(node, state, env, log) {
        if (!actions.hasOwnProperty(node.asttype)) {
            throw new Error('invalid asttype -- ' + node.asttype);
        }
        actions[node.asttype](node, state, env, log);
    }

    function variables(node) {
        /*
         - used, but not defined
         - defined but not used
         - defined more than once
         - shadowing
         - set but not defined
        */
        var symbols = ['true', 'false', 'nil', 'cons', 'car', 'cdr', '+', 'null?', '='];
        var bindings = {};
        symbols.forEach(function(s) {
            bindings[s] = {'position': 'builtin', 'count': 0};
        });
        var state = {},
            env = new environment.Environment(null, bindings),
            log = {'errors': [], 'warnings': []};
        variablesHelp(node, state, env, log);
        return {
            'state': state,
            'env': env,
            'log': log
        };
    }
    
    return variables;
})();


var alphaRenaming = (function() {
/*
var typeDefs = [
    ['Number',      ['numValue'             ]],
    ['Symbol',      ['strValue'             ]],
    ['String',      ['strValue'             ]],
    ['List',        ['elems'                ]],
    ['Dictionary',  ['keyvals'              ]],
    ['Application', ['operator', 'arguments']],
    ['Def',         ['symbol', 'value'      ]],
    ['Set',         ['symbol', 'value'      ]],
    ['Cond',        ['branches', 'elseValue']],
    ['Fn',          ['params', 'forms'      ]],
    ['Beagle',      ['forms'                ]]
];
*/
        // TODO we'll want a name with one part per level of nesting
        //   e.g. v-1 (first top-level var)
        //        v-2-3 (third var, in second enclosed scope)  "{fn {} 3} {fn {v-2-1 v-2-2 *v-2-3*} v-2-3}"
        //   2. something to identify 
    function symbol(node, state, env, log) {
        if (!env.hasBinding(node.strValue)) {
            throw new Error('aborting -- undefined variable');
        }
        var binding = env.getBinding(node.strValue);
        return ast.Symbol(binding);
    }
    
    function list(node, state, env, log) {
        return ast.list(node.elems.map(function(e) {
            return alphaHelp(node, state, env, log);
        });
    }
    
    function application(node, state, env, log) {
        var newOperator = alphaHelp(node.operator, state, env, log),
            newArguments = node.arguments.map(function(a) {
                return alphaHelp(node, state, env, log);
            });
        return ast.application(newOperator, newArguments);
    }
    
    function def(node, state, env, log) {
        if (env.hasOwnBinding(node.symbol.strValue)) {
            throw new Error('aborting -- redefinition of variable');
        }
        var newName = 'v-' + state.map(function(s) {return s.count}).join('-');
        env.addBinding(node.symbol.strValue, newName);
        state[state.length - 1].count++;
        var newValue = alphaHelp(node.value, state, env, log);
        return ast.Def(ast.Symbol(newName), newValue);
    }
    
    function set(node, state, env, log) {
        var newSymbol = alphaHelp(node.symbol),
            newValue = alphaHelp(node.value);
        return ast.Set(newSymbol, newValue);
    }
    
    function cond(node, state, env, log) {
        var newBranches = node.branches.map(function(b) {
                var first = alphaHelp(b[0], state, env, log),
                    second = alphaHelp(b[1], state, env, log);
                return [first, second];
            }),
            newElseValue = alphaHelp(node.elseValue, state, env, log);
        return ast.Cond(newBranches, newElseValue);
    }
    
    function fn(node, state, env, log) {
        state.push({'count': 1});
        var newParams // TODO can't remember if these are javascript strings or AST symbol
        state.pop();
        return ast.Fn(newParams, newForms);
    }
    
    var actions = {
        'Number': function(node) {return node;},
        'Symbol': symbol,
        'String': function(node) {return node;},
        'List'  : list,
//        'Dictionary':  // TODO
        'Application': application,
        'Def'   : def,
        'Set'   : set,
        'Cond'  : cond,
        'Fn'    : fn,
        'Beagle': beagle
    };

    function alphaHelp(node, state, env, log) {
        // TODO
        return 
    }

    function alpha(node) {
        var state = 
            env = 
            log = ;
        return {
            'state': state,
            'env': env,
            'log': log
        };
    }

    return alpha;

})();


module.exports = {
    'variableUsage': variableUsage
};

