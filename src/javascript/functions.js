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

    
    function cons(args) {        
        argsCheck(2, args.length, 'cons');

        var elem = args[0],
            list = args[1];

        if(!(list.type === 'list' || list.type === 'string')) {
            throw new FunctionError('TypeError', 'string or list', list.type, 'cons', "second argument");
        }

        return list.cons(elem);
    };


    function car(args) {
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


    function cdr(args) {         
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
    
    
    function nullQ(args) {
        argsCheck(1, args.length, 'null?');
        
        var list = args[0];

        if(!(list.type === 'list' || list.type === 'string')) {
            throw new FunctionError('TypeError', 'string/list', list.type, 'null?', "only argument");
        } 
        
        return Data.Boolean(list.value.length === 0);
    }


    function list(args) {
        return Data.List(args);
    }


    function eqQ(args) {        
        argsCheck(2, args.length, 'eq?');

        var left = args[0],
            right = args[1];

        var ltype = left.type,
            rtype = right.type,
            lval = left.value,
            rval = right.value;

        if (ltype !== rtype) {
            throw new FunctionError('TypeError', ltype, rtype, "eq?", "arguments must have identical types")
        }

        if (ltype === 'number' || ltype === 'char' || ltype === 'symbol' || ltype === 'boolean') {
            return Data.Boolean(lval === rval);
        }

        throw new FunctionError('TypeError', 'number/char/symbol/boolean', 
                  ltype, "eq?", "can't compare type");
    }


    function plus(args) {        
        argsCheck(2, args.length, '+');

        var left = args[0],
            right = args[1];

        typeCheck('number', left.type, '+', 'first argument');
        typeCheck('number', right.type, '+', 'second argument');
 
        return Data.Number(left.value + right.value);
    }


    function neg(args) {
        argsCheck(1, args.length, 'neg');

        var num = args[0];

        typeCheck('number', num.type, 'neg', 'only argument');

        return Data.Number(-num.value);
    }
    
    
    function primType(args) {
        argsCheck(1, args.length, 'prim-type');
        
        var arg = args[0];
        
        return Data.String(arg.type);
    }
    
    
    function numberLessThan(args) {
        argsCheck(2, args.length, 'number-<');
        
        var l = args[0],
            r = args[1];
        
        typeCheck('number', l.type, 'number-<', 'first argument');
        typeCheck('number', r.type, 'number-<', 'second argument');
        
        return Data.Boolean(l.value < r.value);
    }
    
    
    function data(args) {
        argsCheck(1, args.length, 'data');
        
        var usertype = args[0];
        
        typeCheck('string', usertype.type, 'data', 'only arg');

        function constructor(c_args) {
            argsCheck(1, c_args.length, usertype.value + ' constructor');
            
            return Data.UserDefined(usertype, c_args[0]);
        }
        
        return Data.Function(constructor);
    }
    
    
    function udtType(args) {
        argsCheck(1, args.length, 'udt-type');
        
        var obj = args[0];
        
        typeCheck('userdefined', obj.type, 'udt-type', 'only arg');
        
        return obj.usertype;
    }
    
    
    function udtValue(args) {
        argsCheck(1, args.length, 'udt-value');
        
        var obj = args[0];
        
        typeCheck('userdefined', obj.type, 'udt-value', 'only arg');
        
        return obj.value;
    }


    return {
        'cons'      :  cons,
        'car'       :  car,
        'cdr'       :  cdr,
        'list'      :  list,
        'eq?'       :  eqQ,
        '+'         :  plus,
        'neg'       :  neg,
        'prim-type' :  primType,
        'null?'     :  nullQ,
        'number-<'  :  numberLessThan,
        'data'      :  data,
        'udt-type'  :  udtType,
        'udt-value' :  udtValue
    };

})(Data);