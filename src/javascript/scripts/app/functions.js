define(["app/data"], function (Data) {
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


    var negate = Data.Function(
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


    var primType = Data.Function(
        [null],
        'type',
        function(args) {
            return Data.makeCharList(args[0].type); // TODO this could even be 'intern'-ed for efficiency
        }
    );
    

    return {
        'cons'      :  cons,
        'car'       :  car,
        'cdr'       :  cdr,
        'null?'     :  nullQ,
        '+'         :  plus,
        'neg'       :  negate, // TODO change to negate
        'number-<'  :  numberLessThan, // TODO change to prim-< and implement for char also
        'eq?'       :  eqQ,
        'type'      :  primType // TODO change to prim-type
    };

});