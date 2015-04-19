'use strict';


function MyNumber(num) {
    this.numValue = num;
}

function MySymbol(str) {
    this.name = str;
}

function MyString(str) {
    this.strValue = str;
}

function MyList(elems) {
    this.elems = elems;
}

function MyDict(keyvals) {
    this.keyvals = keyvals;
}

function MyApplication(op, args) {
    console.log('app -- ' + JSON.stringify([op, args]));
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

function MyBeagle(forms) {
    this.forms = forms;
}


module.exports = {
    'Number'     :  MyNumber        ,
    'String'     :  MyString        ,
    'Symbol'     :  MySymbol        ,
    'List'       :  MyList          ,
    'Dict'       :  MyDict          ,
    'Application':  MyApplication   ,
    'Define'     :  MyDef           ,
    'Set'        :  MySet           ,
    'Cond'       :  MyCond          ,
    'Fn'         :  MyFn            ,
    'Beagle'     :  MyBeagle        ,
};

