var Functions = (function (Data) {
    "use strict";

    
    var cons = Data.Function(
        [null, 'list'],
        'cons',
        function (args) {        
            var elem = args[0],
                list = args[1],
                newList, i;
    
            newList = [elem];
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
    
    
    var COMPARABLE = {
        'number' : 1,
        'char'   : 1,
        'symbol' : 1,
        'boolean': 1
    };


    var eqQ = Data.Function(
        [null, null],
        'eq?',
        function(args) {        
            var left = args[0],
                right = args[1],    
                ltype = left.type,
                rtype = right.type;
            if (ltype !== rtype) {
                throw Data.FunctionError('TypeError', ltype, rtype, "eq?", "arguments must have identical types");
            }
            if(!(ltype in COMPARABLE)) {
                throw Data.FunctionError('TypeError', 'number/char/symbol/boolean', 
                      ltype, "eq?", "can't compare type");
            }
            if(!(rtype in COMPARABLE)) {
                throw Data.FunctionError('TypeError', 'number/char/symbol/boolean', 
                      rtype, "eq?", "can't compare type");
            }
            return Data.Boolean(args[0].value === args[1].value);
        }
    );


    var plus = Data.Function(
        ['number', 'number'],
        '+',
        function(args) {
            return Data.Number(args[0].value + args[1].value);
        }
    );


    var neg = Data.Function(
        ['number'],
        'neg',
        function(args) {
            return Data.Number(-args[0].value);
        }
    );
    
    
    var numberLessThan = Data.Function(
        ['number', 'number'],
        'number-<',
        function(args) {
            return Data.Boolean(args[0].value < args[1].value);
        }
    );
    
    
    // does this belong elsewhere? (in Data?)
    var BUILT_INS = {
        'number'      : 1,
        'char'        : 1,
        'boolean'     : 1,
        'symbol'      : 1,
        'list'        : 1,
        'function'    : 1,
        'specialform' : 1,
        'application' : 1,
        'null'        : 1  // not sure if I need this
    };
    
    
    var datum = Data.Function(
        ['list', null],
        'datum',
        function(args) {
            var typeList = args[0],
                type = typeList.value.map(function(c) {// need to get value right?
                    if(c.type !== 'char') {
                        throw Data.FunctionError('TypeError', "list of chars", 'list with ' + c.type, 'datum', '1st arg');
                    }
                    return c.value;
                }).join('');
                value = args[1];
                
            if(type.length === 0) {
                throw Data.FunctionError('ValueError', "non-empty type", "empty type", "datum", '1st arg');
            }
            
            if(type in BUILT_INS) {
            	throw Data.FunctionError("ValueError", 'non built-in type', type, 'datum', "1st arg");
            }
            
            return Data.Datum(type, value);
        }
    );
    
    
    var type = Data.Function(
        [null],
        'type',
        function(args) {
            return Data.makeCharList(args[0].type);
        }
    );
    
    
    var value = Data.Function(
        [null],
        'value',
        function(args) {
            var obj = args[0];
            if(obj.type in BUILT_INS) {
                throw Data.FunctionError('TypeError', 'non built-in type', obj.type, 'value', "1st arg");
            }
            return obj.value;
        }
    );


    return {
        'cons'      :  cons,
        'car'       :  car,
        'cdr'       :  cdr,
        'null?'     :  nullQ,
        '+'         :  plus,
        'neg'       :  neg,
        'number-<'  :  numberLessThan,
        'eq?'       :  eqQ,
        'type'      :  type,
        'datum'     :  datum,
        'value'     :  value
    };

})(Data);