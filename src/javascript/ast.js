var AST = (function() {

    function myError(type, func, expected, actual) {
        return {
            type: type,
            'function': func,
            expected: expected,
            actual: actual,
            module: 'ast'
        };
    }

    function myNumber(num, meta) {
        if(typeof num !== 'number') {
            throw myError('TypeError', 'number', 'number', typeof num);
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
            throw myError('TypeError', 'symbol', 'string', typeof str);
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
            throw myError('TypeError', 'char', 'string', typeof chr);
        }
        if(chr.length !== 1) {
            throw myError('ValueError', 'char', 'length of 1', chr.length);
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
            throw myError('TypeError', 'list', 'array', typeof elems);
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
            throw myError('TypeError', 'application', 'astnode', op.type);
        }
        if(args.length === undefined || typeof args === 'string') {
            throw myError('TypeError', 'application', 'array', typeof args);
        }
        return {
            type       :  'astnode',
            asttype    :  'application',
            'operator' :  op,
            'arguments':  args,
            meta       :  meta
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
            type     :  'astnode',
            asttype  :  'objectliteral',
            entries  :  pairs,
            meta     :  meta
        };
    }

    function myDefine(symbol, value, meta) {
        if(typeof(symbol) !== 'string') {
            throw myError('TypeError', 'define', 'string', typeof symbol);
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
            throw myError('TypeError', 'set!', 'string', typeof symbol);
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
            throw myError('TypeError', 'cond', 'array', typeof branches);
        }
        for(i = 0; i < branches.length; i++) {
            br = branches[i];
            if(br.length === undefined || typeof br === 'string') {
                throw myError('TypeError', 'cond', 'array', typeof br);
            }
            if(br.length !== 2) {
                throw myError('ValueError', 'cond', 'length of 2', br.length);
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
        var names = {}, s, i;
        if(params.length === undefined || typeof params === 'string') {
            throw myError('TypeError', 'lambda', 'array', typeof params);
        }
        for(i = 0; i < params.length; i++) {
            s = params[i];
            if(typeof s !== 'string') {
                throw myError('TypeError', 'lambda', 'string', typeof s);
            }
            if(s in names) {
                throw myError('ValueError', 'lambda', 'unique parameter names', s);
            }
            names[s] = 1;
        }
        if(bodies.length === undefined || typeof bodies === 'string') {
            throw myError('TypeError', 'lambda', 'array', typeof bodies);
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