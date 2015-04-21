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

var nil = Object.create({'type': 'nil'});

function List(elems) {
    this._elems = elems;
}

function Dict(keyvals) {
    this._keyvals = keyvals;
}

function UserFunction(params, forms, env, name) {
    this.params = params;
    this.forms = forms;
    this.env = env;
    this.fname = name; // `fname` so as to avoid over-riding builtins
}

UserFunction.prototype.fapply = function(args) {
    var newEnv = Environment.Environment(env, {}),
        retVal = nil;
    if (args.length !== params.length) {
        // TODO then something bad should happen
    }
    // put parameter bindings into local environment
    for (var j = 0; j < this.params.length; j++) {
        newEnv.addBinding(names[j], c_args[j]);
    }
    // evaluate all the body forms
    for(var k = 0; k < bodies.length; k++) {
        retVal = evaluate(bodies[k], newEnv);
    }
    // and return the last form
    return retVal;
};

UserFunction.prototype.description = function() {
    return ' '.join(['<user-function:', this.fname, '>']); // TODO add line/col position
};

module.exports = {
    'Number'    : MyNumber  ,
    'String'    : MyString  ,
    'Boolean'   : MyBoolean ,
    'Function'  : MyFunction,
    'Nil'       : nil       ,
    'List'      : List      ,
    'Dict'      : Dict      ,
};

