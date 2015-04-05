"use strict";


function MyNumber(value) {
    this._value = value;
}

function MyString(value) {
    this._value = value;
}

function MyBoolean(value) {
    this._value = value;
}

function MyFunction(name, params, forms) {
    this._name = name;
    this._params = params;
    this._forms = forms;
}

function Null() {}

function List(elems) {
    this._elems = elems;
}

function Dict(keyvals) {
    this._keyvals = keyvals;
}

module.exports = {
    'Number'    : MyNumber  ,
    'String'    : MyString  ,
    'Boolean'   : MyBoolean ,
    'Function'  : MyFunction,
    'Null'      : new Null(),    
    'List'      : List      ,
    'Dict'      : Dict      ,
};

