'use strict';


function MyNumber(num) {
    this.value = num;
}

function MySymbol(str) {
    this.value = str;
}

function MyString(str) {
    this.value = str;
}

function MyList(elems) {
    this.elems = elems;
}

function MyDict(keyvals) {
    this.keyvals = keyvals;
}

function MyApplication(op, args) {
    this.operator = op;
    this.arguments = args;
}

function MyDef(symbol, value) {
    this.symbol = symbol;
    this.value = value;
}

function MySet(symbol, value) {
    this.symbol = symbol;
    this.value = value;
}

function MyCond(branches, elseValue) {
    this.branches = branches;
    this.elseValue = elseValue;
}

function MyFn(params, forms) {
    this.params = params;
    this.forms = forms;
}


module.exports = {
    'Number'     :  MyNumber,
    'String'     :  MyString,
    'Symbol'     :  MySymbol,
    'List'       :  MyList,
    'Dict'       :  MyDict,
    'Application':  MyApplication,
    'Define'     :  MyDef,
    'Set'        :  MySet,
    'Cond'       :  MyCond,
    'Fn'         :  MyFn
};

