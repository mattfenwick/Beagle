"use strict";

var Data = require('./data');


var cons = Data.Function(
    [null, 'list'],
    'cons',
    function (args) {
        var elem = args[0],
            list = args[1],
            newList = [elem],
            i;
        for (i = 0; i < list.value.length; i++) {
            newList.push(list.value[i]);
        }
        return new Data.List(newList);
    }
);

var car = Data.Function(
    ['list'],
    'car',
    function (args) {
        var list = args[0];
        if (list.value.length === 0) {
            throw Data.FunctionError("ValueError", "non-empty list", "empty list", 'car', "1st arg");
        }
        return list.value[0];
    }
);

var cdr = Data.Function(
    ['list'],
    'cdr',
    function (args) {
        var list = args[0];
        if (list.value.length === 0) {
            throw Data.FunctionError("ValueError", "non-empty list", "empty list", 'cdr', '1st arg');
        }        
        return Data.List(list.value.slice(1));
    }
);

var nullQ = Data.Function(
    ['list'],
    'null?',
    function (args) {
        var list = args[0];
        return Data.Boolean(list.value.length === 0);
    }
);

var plus = Data.Function(
    ['number', 'number'],
    '+',
    function(args) {
        return Data.Number(args[0].value + args[1].value);
    }
);

var COMPARABLE = {
    'number' : 1,
    'char'   : 1,
    'boolean': 1,
    'nil'    : 1,
};


var isEqual = Data.Function(
    [null, null],
    '=',
    function(args) {
        var left = args[0],
            right = args[1],
            ltype = left.type,
            rtype = right.type;
        if (ltype !== rtype || !COMPARABLE.hasOwnProperty(ltype)) {
            return Data.Boolean(false);
        }
        return Data.Boolean(args[0].value === args[1].value);
    }
);

module.exports = {
    'cons'      :  cons,
    'car'       :  car,
    'cdr'       :  cdr,
    'null?'     :  nullQ,
    '+'         :  plus,
    '='         :  isEqual,
};

