'use strict';

var A = require('./ast');

function number(node) {
    var text = node.int.join(''),
        dec = node.decimal;
    if ( dec ) {
        text += '.' + dec.digits.join('');
    }
    return A.number(parseFloat(text), node._state);
}

var ESCAPES = {'"': '"', '\\': '\\'};

function string(node) {
    var chars = node.body.map(function(c) {
        var ch = c._name === 'escape' ? ESCAPES[c.char] : c.char;
        return A.char(ch, c._state);
    });
    return A.list(chars, node._state);
}

function symbol(node) {
    var name = node.first + node.rest.join('');
    return A.symbol(name, node._state);
}

function list(node) {
    return A.list(node.body.map(build), node._state);
}

function app(node) {
    return A.application(build(node.operator), node.args.map(build), node._state);
}

function define(node) {
    return A.define(build(node.symbol), build(node.form), node._state);
}

function set(node) {
    return A.set(build(node.symbol), build(node.form), node._state);
}

function lambda(node) {
    // what about unique names?
    return A.lambda(node.parameters.map(build), node.forms, node._state);
}

function cond(node) {
    return A.cond(node.pairs, node.else, node._state);
}

var SPECIALS = {
    'cond'  : cond  ,
    'set'   : set   ,
    'define': define,
    'lambda': lambda
};

function spec(node) {
    return SPECIALS[node.value._name](node.value);
}

var NODES = {
    'number' : number,
    'string' : string,
    'symbol' : symbol,
    'list'   : list,
    'app'    : app,
    'spec'   : spec
};

function build(node) {
    return NODES[node._name](node);
}


module.exports = {
    'build': build
};

