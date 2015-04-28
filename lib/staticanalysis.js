'use strict';

var environment = require('./environment');


function symbol(node, state, env, log) {
    var name = node.strValue;
    if (!env.hasBinding(name)) {
        log.errors.push({'name': name, 'position': node._debug.start, 'message': 'usage of undefined variable'});
    } else {
        env.setBinding(name, env.getBinding(name) + 1);
    }
}

function list(node, state, env, log) {
    node.elems.forEach(function(e) {
        variableUsageHelp(e, state, env, log);
    });
}

function application(node, state, env, log) {
    var forms = [node.operator].concat(node.arguments);
    forms.forEach(function(f) {
        variableUsageHelp(f, state, env, log);
    });
}

function def(node, state, env, log) {
    var name = node.symbol.strValue;
    if (env.hasOwnBinding(name)) {
        log.errors.push({'name': name, 'position': node._debug.start, 'message': 'duplicate variable definition'});
    } else if (env.hasBinding(name)) {
        log.warnings.push({'name': name, 'position': node._debug.start, 'message': 'variable shadowing'});
    } else {
        env.addBinding(name, 0);
    }
    // notice how this doesn't recur on node.symbol
    variableUsageHelp(node.value, state, env, log);
}

function set(node, state, env, log) {
    if (!env.hasBinding(node.symbol.strValue)) {
        log.errors.push({'name': node.symbol.strValue, 'position': node._debug.start, 'message': 'setting undefined variable'});
    }
    // we're not going to count 'set' as using a variable
    variableUsageHelp(node.value, state, env, log);
}

function cond(node, state, env, log) {
    node.branches.forEach(function(b) {
        variableUsageHelp(b[0], state, env, log);
        variableUsageHelp(b[1], state, env, log);
    });
    variableUsageHelp(node.elseValue, state, env, log);
}

function fn(node, state, env, log) {
    // push a new env on, add bindings for each of its variables
    var newEnv = new environment.Environment(env, {});
    node.params.forEach(function(p) {
        newEnv.addBinding(p.strValue, 0);
    });
    // go through its children
    node.forms.forEach(function(f) {
        variableUsageHelp(f, state, newEnv, log);
    });
    // now examine the scope's variables, see if they were all used
    node.params.forEach(function(p) {
        if (newEnv.getBinding(p.strValue) === 0) {
            log.warnings.push({'name': p.strValue, 'position': p._debug.start, 'message': 'unused variable'});
        }
    });
}

function beagle(node, state, env, log) {
    node.forms.forEach(function(f) {
        variableUsageHelp(f, state, env, log);
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

function variableUsageHelp(node, state, env, log) {
    if (!actions.hasOwnProperty(node.asttype)) {
        throw new Error('invalid asttype -- ' + node.asttype);
    }
    actions[node.asttype](node, state, env, log);
}

function variableUsage(node) {
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
        bindings[s] = 0;
    });
    var state = {},
        env = new environment.Environment(null, bindings),
        log = {'errors': [], 'warnings': []};
    variableUsageHelp(node, state, env, log);
    return {
        'state': state,
        'env': env,
        'log': log
    };
}

module.exports = {
    'variableUsage': variableUsage
};

