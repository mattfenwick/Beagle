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
                throw new Error(["ValueError", "cannot take car of empty list"]);
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
                throw new Error(["ValueError", "cannot take cdr of empty list"]);
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


    var list = Data.VariadicFunction(
        function (args) {
            return Data.List(args);
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
                throw new Error(['TypeError', ltype, rtype, "eq?", "arguments must have identical types"]);
            }
            if(!(ltype in COMPARABLE)) {
                throw new Error(['TypeError', 'number/char/symbol/boolean', 
                      ltype, "eq?", "can't compare type"]);
            }
            if(!(rtype in COMPARABLE)) {
                throw new Error(['TypeError', 'number/char/symbol/boolean', 
                      rtype, "eq?", "can't compare type"]);
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
    
    
    var datum = Data.Function(
        ['list', null],
        'datum',
        function(args) {
            var typeList = args[0],
                type = typeList.map(function(c) {
                    if(c.type !== 'char') {
                        throw new Error("'datum' requires a list of chars");
                    }
                    return c.value;
                }).join('');
                value = args[1];
                
            if(type.length === 0) {
                throw new Error("datum type may not be empty");
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
    
    
    // does this belong elsewhere?
    var BUILT_INS = {
        'number' : 1,
        'char'   : 1,
        'boolean': 1,
        'symbol' : 1,
        'list'   : 1,
        'function': 1,
        'specialform': 1,
        'null'   : 1  // not sure if I need this
    };
    
    
    var value = Data.Function(
        [null],
        'value',
        function(args) {
            var obj = args[0];
            if(obj.type in BUILT_INS) {
                throw new Error("'value' doesn't work on built-ins (was " + obj.type + " )");
            }
            return obj.value;
        }
    );


    return {
        'cons'      :  cons,
        'car'       :  car,
        'cdr'       :  cdr,
        'list'      :  list,
        'null?'     :  nullQ,
        '+'         :  plus,
        'neg'       :  neg,
        'number-<'  :  numberLessThan,
        'eq?'       :  eqQ
    };

})(Data);