var Data = (function () {
    "use strict";


    function FunctionError(type, expected, actual, fname, message) {
        this.type = type;
        this.expected = expected;
        this.actual = actual;
        this.fname = fname;
        this.message = message;
    }


    FunctionError.prototype.toString = function() {
        return this.type + " in " + this.fname + ": " + this.message + 
               ", expected " + this.expected + " but got " + this.actual;
    };


    function typeCheck(expected, actual, fname, message) {
        if (expected !== actual) {
            throw new FunctionError('TypeError', expected, actual, fname, message);
        }
    }


    function argsCheck(expected, actual, fname, message) {
        if (expected !== actual) {
            throw new FunctionError('NumArgsError', expected, actual, fname, message);
        }
    }
    

    function MyNumber(value) {
        this.value = value;
        this.type = 'number';
    }
    
    
    function Char(value) {
        this.value = value;
        this.type = 'char';
    }


    function Symbol(value) {
        this.value = value;
        this.type = 'symbol';
    }


    function List(value) {
        this.value = value;
        this.type = 'list';
    }


    function MyString(value) {
        this.value = value;
        this.type = 'string';
    }


    function MyFunction(argTypes, name, code) {
        this.argTypes = argTypes;
        this.name = name;
        this.value = code;
        this.type = 'function';
    }
    
    MyFunction.prototype.fapply = function(args) {
        argsCheck(this.argTypes.length, args.length, this.name);
        for(var i = 0; i < this.argTypes.length; i++) {
            if(this.argTypes[i] !== null) {// if it IS null, we don't need to worry about it
            	console.log(JSON.stringify([args, this.argTypes, args[i], this.argTypes[i]]));
                typeCheck(this.argTypes[i], args[i].type, this.name, 'argument ' + (i + 1));
            }
        }
        return this.value(args);
    }
    
    
    function VariadicFunction(code) {
        this.value = code;
        this.type = 'function';
    }
    
    VariadicFunction.prototype.fapply = function(args) {
        return this.value(args);
    }


    function Null() {
        this.type = 'null';
    }


    function SpecialForm(value) {
        this.value = value;
        this.type = 'specialform';
    }


    function MyBoolean(value) {
        this.value = value;
        this.type = 'boolean';
    }
    
    
    function UserDefined(usertype, value) {
        this.usertype = usertype;
        this.value = value;
        this.type = 'userdefined';
    }
    
    
    function MyError(errortype, message, trace) {
        this.errortype = errortype;
        this.message = message;
        this.trace = trace;
        this.type = 'error';
    }


    return {
        'Number': function (x) {
            return new MyNumber(x);
        },
        'Char': function(x) {
            return new Char(x);
        },
        'Function': function (types, name, body) {
            return new MyFunction(types, name, body);
        },
        'VariadicFunction': function(x) {
            return new VariadicFunction(x);
        },
        'List': function (x) {
            return new List(x);
        },
        'Symbol': function (x) {
            return new Symbol(x);
        },
        'Null': function () {
            return new Null();
        },
        'SpecialForm': function (x) {
            return new SpecialForm(x);
        },
        'Boolean': function (x) {
            return new MyBoolean(x);
        },
        'UserDefined': function(x, y) {
            return new UserDefined(x, y);
        },
        'String': function(x) {
            return new MyString(x);
        },
        'Error': function(type, message, trace) {
            return new MyError(type, message, trace);
        }
    };

})();