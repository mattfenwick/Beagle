'use strict';

var A = require('./ast'),
    M = require('unparse-js').maybeerror,
    util = require('util');


function number(cst) {
    var text = cst.int.join('');
    return M.pure(new A.Number(parseInt(text, 10)));
}

var ESCAPES = {'"': '"', '\\': '\\'};

function string(cst) {
    var chars = cst.chars.map(function(c) {
        return c._name === 'escape' ? ESCAPES[c.char] : c.char;
    });
    return M.pure(new A.String(chars));
}

function symbol(cst) {
    var name = cst.first + cst.rest.join('');
    return M.pure(new A.Symbol(name));
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
    return M.pure(new A.List(elems));
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
    return M.pure(new A.Application(op.value, args));
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
    return M.pure(new A.Define(symbolErr.value, formErr.value));
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
    return M.pure(new A.Set(symbolErr.value, formErr.value));
}

function fn(cst) {
    var params = [],
        paramsDict = {};
    for (var i = 0; i < cst.parameters.length; i++) {
        var param = build(cst.parameters[i]); // TODO expecting this to be a symbol
        if (paramsDict.hasOwnProperty(param.name)) {
            return M.error({'message': 'duplicate parameter name', 'name': param.name, 'positions': null}); // TODO -- positions
        }
        params.push(name);
        paramsDict[param.name] = 1;
    }
    var forms = [];
    for (var j = 0; j < cst.forms.length; j++) {
        var formErr = build(cst.forms[j]);
        if (formErr.status !== 'success') {
            return formErr;
        }
        forms.push(formErr.value);
    }
    return M.pure(new A.Fn(params, forms));
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
    return M.pure(new A.Cond(branches, elseForm.value));
}

function beagle(cst) {
    var forms = [];
    for (var i = 0; i < cst.forms.length; i++) {
        var cstNode = cst.forms[i],
            astErr = buildAST(cstNode);
        if (astErr.status !== 'success') {
            return astErr;
        }
        forms.push(astErr.value);
    }
    return M.pure(new A.Beagle(forms));
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
    'special': special,
    'beagle' : beagle,
};

function buildAST(node) {
    if (NODES.hasOwnProperty(node._name)) {
        var ast = NODES[node._name](node);
        ast._debug = node['_state']; // TODO will change with unparse-js version
        return ast;
    }
    console.log('invalid node name: ' + util.inspect(node, {'depth': null}));
    throw new Error('invalid node name: ' + node._name);
}


module.exports = {
    'buildAST': buildAST
};

