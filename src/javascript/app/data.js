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


    function List(value) {
        this.value = value;
        this.type = 'list';
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
                // console.log(JSON.stringify([args, this.argTypes, args[i], this.argTypes[i]]));
                typeCheck(this.argTypes[i], args[i].type, this.name, 'argument ' + (i + 1));
            }
        }
        return this.value(args);
    }


    function Null() {
        this.type = 'null';
    }


    function MyBoolean(value) {
        this.value = value;
        this.type = 'boolean';
    }
    
    
    function makeCharList(jsString) {
        var list = [],
            i;
        for(i = 0; i < jsString.length; i++) {
            list.push(new Char(jsString[i]));
        }
        return new List(list);
    }


    return {
        // primitive types
        'Number': function (x) {
            return new MyNumber(x);
        },
        'Char': function(x) {
            return new Char(x);
        },
        'Boolean': function (x) {
            return new MyBoolean(x);
        },
        'Function': function (types, name, body) {
            return new MyFunction(types, name, body);
        },
        
        // composite types
        'List': function (x) {
            return new List(x);
        },
        
        // like () in Haskell??
        'Null': function () {
            return new Null();
        },
        
        // convenience function
        'makeCharList': makeCharList,
        
        // custom errors
        'FunctionError': function(a,b,c,d,e) {
            return new FunctionError(a,b,c,d,e);
        }
        
    };

})();