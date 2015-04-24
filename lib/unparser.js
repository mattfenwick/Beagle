'use strict';

function branch(pair) {
    return '{' + unparse(pair[0]) + ' ' + unparse(pair[1]) + '}';
}

var ACTIONS =  {
        'Number'     :  function(node) {return node.numValue.toString();},
        'Symbol'     :  function(node) {return node.strValue;},
        'String'     :  function(node) {return ['"', node.strValue, '"'].join('');}, // TODO what about escapes?
        'List'       :  function(node) {return '[' + node.elems.map(unparse).join(' ') + ']';},
        'Dictionary' :  function(node) {return node;}, // TODO
        'Application':  function(node) {return '(' + [node.operator].concat(node.arguments).map(unparse).join(' ') + ')';},
        'Def'        :  function(node) {return '{define ' + unparse(node.symbol) + " " + unparse(node.value) + "}";},
        'Set'        :  function(node) {return '{set ' + unparse(node.symbol) + " " + unparse(node.value) + "}";},
        'Cond'       :  function(node) {return '{cond ' + node.branches.map(branch) + " " + unparse(node.elseValue) + "}";},
        'Fn'         :  function(node) {return '{fn {' + node.params.map(unparse).join(' ') + "} " + node.forms.map(unparse).join(' ') + "}";},
        'Beagle'     :  function(node) {return node.forms.map(unparse).join('\n');}
    };

function unparse(node) {
    var type = node.asttype,
        action = ACTIONS[type];
    
    if (action) {
        return action(node);
    }
    
    throw new Error("cannot unparse node:  unrecognized type (" + type + ") in node: " + JSON.stringify(node));
}

module.exports = {
    'unparse':  unparse
};

