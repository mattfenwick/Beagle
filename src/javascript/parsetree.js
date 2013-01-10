var ParseTree = (function() {
    "use strict";
    
    function myNumber(num, meta) {
        if(typeof num !== 'number') {
            throw new Error('type error -- expected number');
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
            throw new Error('type error -- expected string');
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
            throw new Error('type error -- expected string');
        }
        return {
            type      :  'parsenode',
            nodetype  :  'string',
            value     :  str,
            meta      :  meta
        };
    }

    function myObject(table, meta) {
        if(typeof table !== 'object') {
            throw new Error('type error -- expected object');
        }
        return {
            type     :  'parsenode',
            nodetype :  'objectliteral',
            table    :  table,
            meta     :  meta
        };
    }
    
    function myList(elements, meta) {
        if(elements.length === undefined) {
            throw new Error('type error -- expected array');
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
            throw new Error('type error -- expected parsenode');
        }
        if(args.length === undefined) {
            throw new Error('type error -- expected array');
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
            throw new Error('type error -- expected string');
        }
        if(args.length === undefined) {
            throw new Error('type error -- expected array');
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