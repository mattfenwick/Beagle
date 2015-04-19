'use strict';

var A = require('./ast'),
    M = require('unparse-js').maybeerror,
    util = require('util');

function grabDebuggingData(nodeBuilder) {
    return function(cst) {
        var ast = nodeBuilder(cst);
        ast._debug = {
            'position': null, // TODO put something in here
        };
        return ast;
    };
}

function number(cst) {
    var text = cst.int.join('');
    return M.pure(A.Number(parseInt(text, 10)));
}

var ESCAPES = {'"': '"', '\\': '\\'};

function string(cst) {
    var chars = cst.chars.map(function(c) {
        return c._name === 'escape' ? ESCAPES[c.char] : c.char;
    });
    return A.String(chars);
}

function symbol(cst) {
    console.log('builder -- symbol');
    var name = cst.first + cst.rest.join('');
    var out = M.pure(new A.Symbol(name));
    console.log('build -- symbol -- out: ' + util.inspect(out, {depth: null}));
    return out;
}

function list(cst) {
    var elems = [];
    for (var i = 0; i < cst.body.length; i++) {
        var elem = cst.body[i],
            astErr = build(elem);
        if (astErr.status !== 'success' ) {
            return astErr;
        }
        elems.push(astErr.value);
    }
    return A.List(elems);
}

function app(cst) {
    var op = build(cst.operator);
    // TODO check whether `op` is a fn or symbol
    if (op.status !== 'success') {
        return op;
    }
    var args = [];
    for (var i = 0; i < cst.args.length; i++) {
        var arg = cst.args[i],
            argErr = build(arg);
        if (argErr !== 'success') {
            return argErr;
        }
        args.push(argErr.value);
    }
    return A.Application(op.value, args);
}

function def(cst) {
    var symbolErr = build(cst.symbol),
        formErr = build(cst.form);
    if (symbolErr.status !== 'success') {
        return symbolErr;
    }
    if (formErr.status !== 'success') {
        return formErr;
    }
    return A.Define(symbolErr.value, formErr.value);
}

function set(cst) {
    var symbolErr = build(cst.symbol),
        formErr = build(cst.form);
    if (symbolErr.status !== 'success') {
        return symbolErr;
    }
    if (formErr.status !== 'success') {
        return formErr;
    }
    return A.Set(symbolErr.value, formErr.value);
}

function fn(cst) {
    var params = [],
        paramsDict = {};
    for (var i = 0; i < cst.parameters.length; i++) {
        var name = cst.parameters[i]; // TODO do we need to do some kind of extraction?
        if (paramsDict.hasOwnProperty(name)) {
            return; // TODO some kind of error
        }
        params.push(name);
        paramsDict[name] = 1;
    }
    var forms = [];
    for (var j = 0; j < cst.forms.length; j++) {
        var formErr = build(cst.forms[j]);
        if (formErr.status !== 'success') {
            return formErr;
        }
        forms.push(formErr.value);
    }
    return A.Fn(params, forms);
}

function cond(cst) {
    var branches = [];
    for (var i = 0; i < cst.pairs.length; i++) {
        var pred = build(cst.pairs[i][0]),
            result = build(cst.pairs[i][1]);
        if (pred.status !== 'success') {
            return pred;
        }
        if (result.status !== 'success') {
            return result;
        }
        branches.push([pred.value, result.value]);
    }
    var elseForm = build(cst.else);
    if (elseForm.status !== 'success') {
        return elseForm;
    }
    return M.pure(A.Cond(branches, elseForm.value));
}

var SPECIALS = {
    'cond'  : cond  ,
    'set'   : set   ,
    'def'   : def   ,
    'fn'    : fn
};

function special(node) {
    return SPECIALS[node.value._name](node.value);
}

var NODES = {
    'number' : number,
    'string' : string,
    'symbol' : symbol,
    'list'   : list,
    'app'    : app,
    'special': special
};

function buildAST(node) {
    if (NODES.hasOwnProperty(node._name)) {
        return NODES[node._name](node);
    }
    console.log('invalid node name: ' + util.inspect(node, {'depth': null}));
    throw new Error('invalid node name: ' + node._name);
}


module.exports = {
    'buildAST': buildAST
};

