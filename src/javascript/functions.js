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

        typeCheck('list', list.type,  'cons', "second argument");

        var newList = [elem];
        for (var i = 0; i < list.value.length; i++) {
            newList.push(list.value[i]);
        }

        return Data.List(newList);
    };


    function car(args) {
        argsCheck(1, args.length, 'car');

        var list = args[0];
        
        typeCheck('list', list.type, "car", "only argument");
        
        if (list.value.length === 0) {
            throw new FunctionError("ValueError", "non-empty list", "list", 
                  'car', "cannot take car of empty list");
        }
        
        return list.value[0];
    }


    function cdr(args) {         
        argsCheck(1, args.length, 'cdr');
        
        var list = args[0];
        
        typeCheck('list', list.type, 'cdr', 'only argument');
	
        if (list.value.length === 0) {
            throw new FunctionError("ValueError", 'non-empty list', 'list', 
                  'cdr', "cannot take cdr of empty list");
        }
        
        return Data.List(list.value.slice(1));
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

        if (ltype === 'number' || ltype === 'string' || ltype === 'symbol' || ltype === 'boolean') {
            return Data.Boolean(lval === rval);
        }

        throw new FunctionError('TypeError', 'number/string/symbol/boolean', 
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
    
    
    function nullQ(args) {
    	argsCheck(1, args.length, 'null?');
    	
    	var list = args[0];
    	
    	typeCheck('list', list.type, 'null?', 'only argument');
    	
    	return Data.Boolean(list.value.length === 0);
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
        'null?'     :  nullQ
    };

})(Data);