var Functions = (function (Data) {
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

    
    var cons = Data.UncheckedFunction(
        function (args) {        
            argsCheck(2, args.length, 'cons');
    
            var elem = args[0],
                list = args[1],
                newList, i;
    
            if(list.type === 'list') {
                newList = [elem];
                for (i = 0; i < list.value.length; i++) {
                    newList.push(list.value[i]);
                }
    
                return new Data.List(newList);
            }
            
            if(list.type === 'string') {
                typeCheck('char', elem.type, 'cons', "string 'cons' requires a char as 1st arg");
    
                return new Data.String(elem.value + list.value);
            }
            
            throw new FunctionError('TypeError', 'string or list', list.type, 'cons', "second argument");
        }
    );


    var car = Data.UncheckedFunction(
        function (args) {
            argsCheck(1, args.length, 'car');
    
            var list = args[0], fst;
    
            if(!(list.type === 'list' || list.type === 'string')) {
                throw new FunctionError('TypeError', 'string/list', list.type, 'car', "only argument");
            }
            
            if (list.value.length === 0) {
                throw new FunctionError("ValueError", "non-empty", "empty", 
                      'car', "cannot take car of empty list/string");
            }
            
            fst = list.value[0];
            if(list.type === 'list') {
                return fst;
            } else { // it's a string, so the first is a char
                return Data.Char(fst);
            }
        }
    );


    var cdr = Data.UncheckedFunction(
        function (args) {         
            argsCheck(1, args.length, 'cdr');
            
            var list = args[0], 
                rest;
            
            if(!(list.type === 'list' || list.type === 'string')) {
                throw new FunctionError('TypeError', 'string/list', list.type, 'cdr', "only argument");
            } 
       
            if (list.value.length === 0) {
                throw new FunctionError("ValueError", 'non-empty', 'empty', 
                      'cdr', "cannot take cdr of empty list/string");
            }
            
            rest = list.value.slice(1);
            if (list.type === 'list') {
                return Data.List(rest);
            } else { // it's a string
                return Data.String(rest);
            }
        }
    );
    
    
    var nullQ = Data.Function(
        1,
        function(args) {
            return (args[0].type === 'list' || args[0].type === 'string');
        },
        'null?',
        function (args) {             
            var list = args[0];
            
            return Data.Boolean(list.value.length === 0);
        }
    );


    var list = Data.UncheckedFunction(
        function (args) {
            return Data.List(args);
        }
    );
    
    
    var COMPARABLE = {
        'number' : 1,
        'char'   : 1,
        'symbol' : 1,
        'boolean': 1,
        'string' : 1
    };


    var eqQ = Data.Function(
        2,
        function(args) {        
            var left = args[0],
                right = args[1],    
                ltype = left.type,
                rtype = right.type;
            if (ltype !== rtype) {
                throw new FunctionError('TypeError', ltype, rtype, "eq?", "arguments must have identical types");
            }
            if(!(ltype in COMPARABLE)) {
                throw new FunctionError('TypeError', 'number/char/symbol/boolean/string', 
                      ltype, "eq?", "can't compare type");
            }
        },
        'eq?',
        function (args) {
            return Data.Boolean(args[0].value === args[1].value);
        }
    );


    var plus = Data.Function(
        2,
        function(args) {
            typeCheck('number', args[0].type, '+', 'first argument');
            typeCheck('number', args[1].type, '+', 'second argument');
        },
        'plus',
        function(args) {
            return Data.Number(args[0].value + args[1].value);
        }
    );


    var neg = Data.Function(
        1,
        function(args) {
            typeCheck('number', args[0].type, 'neg', 'only argument');
        },
        'neg',
        function neg(args) {
            return Data.Number(-args[0].value);
        }
    );
    
    
    var primType = Data.Function(
        1,
        function(args) {},
        'prim-type',
        function(args) {
            return Data.String(args[0].type);
        }
    );
    
    
    var numberLessThan = Data.Function(
        2,
        function(args) {
            typeCheck('number', args[0].type, 'number-<', 'first argument');
            typeCheck('number', args[1].type, 'number-<', 'second argument');
        },
        'number-<',
        function numberLessThan(args) {
            return Data.Boolean(args[0].value < args[1].value);
        }
    );
    
    
    var data = Data.Function(
        1,
        function(args) {         
            typeCheck('string', args[0].type, 'data', 'only arg');
        },
        'data',
        function(args) {
            return Data.Function(
                1,
                function() {},
                usertype.value + ' constructor',
                function(c_args) {
                    return Data.UserDefined(args[0], c_args[0]);
                }
            )
        }
    );
    
    
    var udtType = Data.Function(
        1,
        function(args) {
            typeCheck('userdefined', args[0].type, 'udt-type', 'only arg');
        },
        'udt-type',
        function(args) {
            return args[0].usertype;
        }
    );
    
    
    var udtValue = Data.Function(
        1,
        function(args) {
            typeCheck('userdefined', args[0].type, 'udt-value', 'only arg');
        },
        'udt-value',
        function(args) {
            return args[0].value;
        }
    );
    
    
    var errorMessage = Data.Function(
        1,
        function(args) {
            typeCheck('error', args[0].type, 'error-message', 'only arg');
        },
        'error-message',
        function(args) {
            return args[0].message;
        }
    );
    
    
    function errorType(args) {
        argsCheck(1, args.length, 'error-type');
        
        var err = args[0];
        
        typeCheck('error', err.type, 'error-type', 'only arg');
        
        return err.errortype;
    }
    
    
    function errorTrace(args) {
        argsCheck(1, args.length, 'error-trace');
        
        var err = args[0];
        
        typeCheck('error', err.type, 'error-trace', 'only arg');
        
        return err.trace;
    }
    
    
    function newError(args) {
        argsCheck(3, args.length, 'Error');
        
        var errortype = args[0],
            message = args[1],
            trace = args[2];
        
        typeCheck('string', errortype.type, 'Error', '1st arg');
        typeCheck('string', message.type, 'Error', '2nd arg');
        
        return Data.Error(errortype, message, trace);
    }


    return {
        'cons'      :  cons,
        'car'       :  car,
        'cdr'       :  cdr,
        'list'      :  list,
        'null?'     :  nullQ,
        '+'         :  plus,
        'neg'       :  neg,
        'number-<'  :  numberLessThan,
        'eq?'       :  eqQ,
        'prim-type' :  primType,
        'data'      :  data,
        'udt-type'  :  udtType,
        'udt-value' :  udtValue,
        'error-trace'    :  errorTrace,
        'error-message'  :  errorMessage,
        'error-type'     :  errorType,
        'Error'          :  newError
    };

})(Data);