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
                throw Data.FunctionError('TypeError', 'number/char/boolean', 
                      ltype, "eq?", "can't compare type");
            }
            if(!(rtype in COMPARABLE)) { // TODO:  isn't this test unnecessary?
                throw Data.FunctionError('TypeError', 'number/char/boolean', 
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
    
    
    var object = Data.Function(
        ['list'],
        'object',
        function(args) {
            var table = {},
                pairs = args[0];
            pairs.value.map(function(pair) {
                if(pair.type !== 'list') {
                    throw new Data.FunctionError('TypeError', 'list', pair.type,
                        'object', "'object' needs list of pairs");
                }
                if(pair.value.length !== 2) {
                    throw new Data.FunctionError('ObjectError', 'key-value pairs',
                          'list of length ' + pair.length, 'object', 'when building an object');
                }
                if(pair.value[0].type !== 'list') {
                    throw new Data.FunctionError('TypeError', 'list', pair.value[0].type,
                        'object', 'object keys must be lists of characters');
                }
                var key = pair.value[0].value.map(function(c) {
                        if(c.type !== 'char') {
                            throw Data.FunctionError('TypeError', "list of chars", 
                                'list with ' + c.type, 'object', 'object key');
                        }
                        return c.value;
                    }).join('');
                table[key] = pair.value[1]; // what about duplicate keys?  what about empty keys?
            });
            return Data.Object(table);
        }
    );
    
    
    // does this belong elsewhere? (in Data?)
    var BUILT_INS = {
        'number'      : 1,
        'char'        : 1,
        'boolean'     : 1,
        'list'        : 1,
        'function'    : 1,
        'object'      : 1,
        'specialform' : 1, // do I need this?
        'null'        : 1  // not sure if I need this
    };

    
    var type = Data.Function(
        [null],
        'type',
        function(args) {
            return Data.makeCharList(args[0].type);
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
        'object'    :  object,
        'type'      :  type
    };

})(Data);