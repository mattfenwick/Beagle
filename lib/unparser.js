'use strict';

function cond(node) {
    var pairs = node.pairs;
    return '{cond ' + unparse(pairs) + " " + unparse(node.elseValue) + "}";
}

function list(node) {
    return '[' + node.elements.map(unparse).join(' ') + ']';
}

function app(node) {
    return '(' + [node.operator].concat(node.arguments).map(unparse).join(' ') + ')';
}

function lambda(node) {
	return '{lambda ' + [].concat(node.parameters, node.bodyForms, node.lastForm).map(unparse).join(' ') + "}";
}

var ACTIONS =  {
        'symbol'     :  function(node) {return node.value.toString();},
        'number'     :  function(node) {return node.value.toString();},
        'char'       :  function(node) {return "'" + node.value.toString() + "'";},
        'list'       :  list,
        'application':  app,
        'cond'       :  cond,
        'lambda'     :  lambda,
        'define'     :  function(node) {return '{define ' + unparse(node.symbol) + " " + unparse(node.astnode) + "}";},
        'set'       :  function(node) {return '{set ' + unparse(node.symbol) + " " + unparse(node.astnode) + "}";}
    };

function unparse(node) {
    var type = node.asttype,
        action;
    
    if(action = ACTIONS[type]) {
        return action(node);
    }
    
    throw new Error("cannot unparse node:  unrecognized type (" + type + ")");
}

module.exports = {
    'unparse':  unparse
};

