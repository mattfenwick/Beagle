var ParseTree = (function() {
    "use strict";
    
    function myError(type, func, expected, actual) {
        return {
            type    : type,
            module  : 'parsetree',
            'function': func,
            expected : expected,
            actual  : actual
        };
    }
    
    function myNumber(num, meta) {
        if(typeof num !== 'number') {
            throw myError('TypeError', 'number', 'number', typeof num);
        }
        return {
            type     :  'parsenode',
            nodetype :  'number',
            value      :  num,
            meta       :  meta
        };
    }
    
    function mySymbol(sym, meta) {
        if(typeof sym !== 'string') {
            throw myError('TypeError', 'symbol', 'string', typeof sym);
        }
        return {
            type     :  'parsenode',
            nodetype :  'symbol',
            value      :  sym,
            meta       :  meta
        };
    }
    
    function myString(str, meta) {
        if(typeof str !== 'string') {
            throw myError('TypeError', 'string', 'string', typeof str);
        }
        return {
            type      :  'parsenode',
            nodetype  :  'string',
            value     :  str,
            meta      :  meta
        };
    }

    function myObject(pairs, meta) {
        if(pairs.length === undefined || typeof pairs === 'string') {
            throw myError('TypeError', 'object', 'array', typeof pairs);
        }
        var i, pair;
        pairs.map(function(pair) {
            if(pair.length === undefined || typeof pair === 'string') {
                throw myError('TypeError', 'object', 'array', typeof pair);
            }
            if(pair.length !== 2) {
                throw myError('ValueError', 'object', 'length of 2', pair.length);
            }
            if(typeof pair[0] !== 'string') {
                throw myError('TypeError', 'object', 'string', typeof pair[0]);
            }
        });
        return {
            type     :  'parsenode',
            nodetype :  'objectliteral',
            entries  :  pairs,
            meta     :  meta
        };
    }
    
    function myList(elements, meta) {
        if(elements.length === undefined || typeof elements === 'string') {
            throw myError('TypeError', 'list', 'array', typeof elements);
        };
        return {
            type      :  'parsenode',
            nodetype  :  'listliteral',
            elements  :  elements,
            meta      :  meta
        };
    }
    
    function myApp(op, args, meta) {
        if(op.type !== 'parsenode') {
            throw myError('TypeError', 'application', 'parsenode', op.type);
        }
        if(args.length === undefined || typeof args === 'string') {
            throw myError('TypeError', 'application', 'array', typeof args);
        }
        return {
            type       :  'parsenode',
            nodetype   :  'application',
            'operator' :  op,
            'arguments':  args,
            meta       :  meta
        };
    }
    
    function mySpecial(op, args, meta) {
        if(typeof op !== 'string') {
            throw myError('TypeError', 'special', 'string', typeof op);
        }
        if(args.length === undefined || typeof args === 'string') {
            throw myError('TypeError', 'special', 'array', typeof args);
        }
        return {
            type       :  'parsenode',
            nodetype   :  'special',
            'operator' :  op,
            'arguments':  args,
            meta       :  meta
        };
    }


    return {
        'number'  :  myNumber,
        'symbol'  :  mySymbol,
        'string'  :  myString,
        'app'     :  myApp, // not sure if I like the 'app' abbreviation
        'list'    :  myList,
        'object'  :  myObject,
        'special' :  mySpecial
    };

})();