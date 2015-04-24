'use strict';

var A = require('./ast'),
    constructors = A.constructors,
    M = require('unparse-js').maybeerror,
    util = require('util');


function number(cst) {
    var text = cst.int.join('');
    return M.pure(constructors.Number(parseInt(text, 10)));
}

var ESCAPES = {'"': '"', '\\': '\\'};

function string(cst) {
    var chars = cst.chars.map(function(c) {
        return c._name === 'escape' ? ESCAPES[c.char] : c.char;
    });
    return M.pure(constructors.String(chars));
}

function symbol(cst) {
    var name = cst.first + cst.rest.join('');
    return M.pure(constructors.Symbol(name));
}

function list(cst) {
    var elems = [];
    for (var i = 0; i < cst.body.length; i++) {
        var elem = cst.body[i],
            astErr = buildAST(elem);
        if (astErr.status !== 'success' ) {
            return astErr;
        }
        elems.push(astErr.value);
    }
    return M.pure(constructors.List(elems));
}

function app(cst) {
    var opErr = buildAST(cst.operator);
    // TODO check whether `op` is a fn or symbol
    if (opErr.status !== 'success') {
        return opErr;
    }
    var args = [];
    for (var i = 0; i < cst.args.length; i++) {
        var arg = cst.args[i],
            argErr = buildAST(arg);
        if (argErr.status !== 'success') {
            return argErr;
        }
        args.push(argErr.value);
    }
    return M.pure(constructors.Application(opErr.value, args));
}

function def(cst) {
    var symbolErr = buildAST(cst.symbol),
        formErr = buildAST(cst.form);
    if (symbolErr.status !== 'success') {
        return symbolErr;
    }
    if (formErr.status !== 'success') {
        return formErr;
    }
    return M.pure(constructors.Def(symbolErr.value, formErr.value));
}

function set(cst) {
    var symbolErr = buildAST(cst.symbol),
        formErr = buildAST(cst.form);
    if (symbolErr.status !== 'success') {
        return symbolErr;
    }
    if (formErr.status !== 'success') {
        return formErr;
    }
    return M.pure(constructors.Set(symbolErr.value, formErr.value));
}

function fn(cst) {
//    console.log('fn -- ' + Object.getOwnPropertyNames(cst));
    var params = [],
        paramsDict = {};
    for (var i = 0; i < cst.parameters.length; i++) {
        var paramErr = buildAST(cst.parameters[i]); // TODO expecting this to be a symbol
        if (paramErr.status !== 'success') {
            return paramErr;
        }
        var param = paramErr.value;
//        console.log('fn symbol: ' + JSON.stringify(param));
        if (paramsDict.hasOwnProperty(param.strValue)) {
            return M.error({'message': 'duplicate parameter name', 'name': param.name, 'positions': null}); // TODO -- positions
        }
        params.push(param);
        paramsDict[param.strValue] = 1;
    }
    var forms = [];
    for (var j = 0; j < cst.forms.length; j++) {
        var formErr = buildAST(cst.forms[j]);
        if (formErr.status !== 'success') {
            return formErr;
        }
        forms.push(formErr.value);
    }
    return M.pure(constructors.Fn(params, forms));
}

function cond(cst) {
    var branches = [];
//    console.log('cond -- ' + JSON.stringify(cst, null, 2));
    for (var i = 0; i < cst.pairs.length; i++) {
//        console.log('cond pair -- ' + JSON.stringify(cst.pairs[i]));
        var pred = buildAST(cst.pairs[i].condition),
            result = buildAST(cst.pairs[i].result);
        if (pred.status !== 'success') {
            return pred;
        }
        if (result.status !== 'success') {
            return result;
        }
        branches.push([pred.value, result.value]);
    }
    var elseForm = buildAST(cst.else);
    if (elseForm.status !== 'success') {
        return elseForm;
    }
    return M.pure(constructors.Cond(branches, elseForm.value));
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
    return M.pure(constructors.Beagle(forms));
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
//        if (false) { // uncomment to get rid of debugging information
        if (ast.status === 'success') {
            ast.value._debug = {
                'start': node['_start'],
                'end': node['_end']
            };
        }
        return ast;
    }
//    console.log('invalid node name: ' + util.inspect(node, {'depth': null}));
    throw new Error('invalid node name: ' + node._name);
}


module.exports = {
    'buildAST': buildAST
};

