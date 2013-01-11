var AST = (function() {

    function myNumber(num, meta) {
        if(typeof num !== 'number') {
            throw new Error('type error -- expected number');
        }
        return {
            type     :  'astnode',
            asttype  :  'number',
            value    :  num,
            meta     :  meta
        };
    }
    
    function mySymbol(str, meta) {
        if(typeof str !== 'string') {
            throw new Error('type error -- expected string');
        }
        return {
            type     :  'astnode',
            asttype  :  'symbol',
            value    :  str,
            meta     :  meta
        };
    }
    
    function myChar(chr, meta) {
        if(typeof chr !== 'string') {
            throw new Error('type error -- expected string');
        }
        if(chr.length !== 1) {
            throw new Error('value error -- expected length of 1');
        }
        return {
            type     :  'astnode',
            asttype  :  'char',
            value    :  chr,
            meta     :  meta
        };
    }
    
    function myList(elems, meta) {
        if(elems.length === undefined || typeof(elems) === 'string') {
            throw new Error('type error -- expected array');
        }
        return {
            type:  'astnode',
            asttype: 'listliteral',
            elements: elems,
            meta: meta
        };
    }
    
    function myApplication(op, args, meta) {
        if(op.type !== 'astnode') {
            throw new Error('type error -- expected astnode');
        }
        if(args.length === undefined || typeof args === 'string') {
            throw new Error('type error -- expected array');
        }
        return {
            type       :  'astnode',
            asttype    :  'application',
            'operator' :  op,
            'arguments':  args,
            meta       :  meta
        };
    }
    
    function myObject(table, meta) {
        if(typeof table !== 'object') {
            throw new Error('type error -- expected object');
        }
        return {
            type     :  'astnode',
            asttype  :  'objectliteral',
            table    :  table,
            meta     :  meta
        };
    }

    function myDefine(symbol, value, meta) {
        if(typeof(symbol) !== 'string') {
            throw new Error('type error -- expected string');
        }
        return {
            type    : 'astnode',
            asttype : 'define',
            symbol  : symbol, 
            value   : value,
            meta    : meta
        };
    }

    function mySetBang(symbol, value, meta) {
        if(typeof(symbol) !== 'string') {
            throw new Error('type error -- expected string');
        }
        return {
            type    : 'astnode',
            asttype : 'set!',
            symbol  : symbol, 
            value   : value,
            meta    : meta
        };
    }

    function myCond(branches, elseValue, meta) {
        var i, br;
        if(branches.length === undefined || typeof branches === 'string') {
            throw new Error('type error -- expected array');
        }
        for(i = 0; i < branches.length; i++) {
            br = branches[i];
            if(br.length === undefined || typeof br === 'string') {
                throw new Error('type error -- expected array');
            }
            if(br.length !== 2) {
                throw new Error('value error -- expected array of length 2');
            }
        }
        return {
            type     : 'astnode',
            asttype  : 'cond',
            branches : branches, 
            elseValue: elseValue,
            meta     : meta
        };
    }

    function myLambda(params, bodies, returnValue, meta) {
        var names = {}, s;
        if(params.length === undefined || typeof params === 'string') {
            throw new Error('type error -- expected array of parameters');
        }
        for(i = 0; i < params.length; i++) {
            s = params[i];
            if(typeof(s) !== 'string') {
                throw new Error('type error -- expected string');
            }
            if(s in names) {
                throw new Error("value error -- duplicate parameter name (" + s + ")");
            }
            names[s] = 1;
        }
        if(bodies.length === undefined || typeof bodies === 'string') {
            throw new Error('type error -- expected array of bodies');
        }
        return {
            type: 'astnode',
            asttype: 'lambda',
            parameters: params, 
            bodies: bodies,
            returnValue: returnValue,
            meta: meta
        };
    }
    

    return {
        'number'     :  myNumber,
        'char'       :  myChar,
        'symbol'     :  mySymbol,
        'list'       :  myList,
        'object'     :  myObject,
        'application':  myApplication,
        'define'     :  myDefine,
        'setBang'    :  mySetBang,
        'cond'       :  myCond,
        'lambda'     :  myLambda
    };

})();