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
        function neg(args) {
            return Data.Number(-args[0].value);
        }
    );
    
    
    var primType = Data.Function(
        [null],
        'prim-type',
        function(args) {
            return Data.String(args[0].type);
        }
    );
    
    
    var numberLessThan = Data.Function(
        ['number', 'number'],
        'number-<',
        function numberLessThan(args) {
            return Data.Boolean(args[0].value < args[1].value);
        }
    );
    
    
    var data = Data.Function(
        [null],
        'data',
        function(args) {
            return Data.Function(
                [null],
                args[0].value + ' constructor', // do we need to convert 'usertype.value' to a string?
                function(c_args) {
                    return Data.UserDefined(args[0], c_args[0]);
                }
            );
        }
    );
    
    
    var udtType = Data.Function(
        ['userdefined'],
        'udt-type',
        function(args) {
            return args[0].usertype;
        }
    );
    
    
    var udtValue = Data.Function(
        ['userdefined'],
        'udt-value',
        function(args) {
            return args[0].value;
        }
    );
    
    
    var errorMessage = Data.Function(
        ['error'],
        'error-message',
        function(args) {
            return args[0].message;
        }
    );
    
    
    var errorType = Data.Function(
        ['error'],
        'error-type',
        function(args) {
            return args[0].errortype;
        }
    );
    
    
    var errorTrace = Data.Function(
        ['error'],
        'error-trace',
        function(args) {
            return args[0].trace;
        }
    );
    
    
    var newError = Data.Function(
        ['error', null, null],
        'Error',
        function(args) {
            var errortype = args[0],
                message = args[1],
                trace = args[2];
        
            return Data.Error(errortype, message, trace);
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