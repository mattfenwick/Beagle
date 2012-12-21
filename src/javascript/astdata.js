var ASTData = (function(ME) {

    function astNode(asttype, value) {
        return {
            type   :  'astnode',
            asttype:  asttype,
            value  :  value
        };
    }

    function myNumber(num) {
        if(typeof(num) !== 'number') {
            return ME.error('type error');
        }
        return ME.pure(astNode('number', num));
    }
    
    function mySymbol(string) {
        if(typeof(string) !== 'string') {
            return ME.error('type error');
        };
        return ME.pure(astNode('symbol', string));
    }
    
    function myChar(chr) {
        if(typeof(chr) !== 'string' || chr.length !== 1) {
            return ME.error('type/value error');
        };
        return ME.pure(astNode('char', chr));
    }
    
    function myList(elems) {
        if(elems.length === undefined || typeof(elems) === 'string') {
            return ME.error('type error');
        }
        return ME.pure(astNode('list', elems));
    }
    
    function myApplication(op, args) {
        return ME.pure(astNode('application', {operator: op, 'arguments': args}));
    }
    
    function myObject(pairs) {
        var table = {}, i, p;
        for(i = 0; i < pairs.length; i++) {
            p = pairs[i];
            key = p[0];
            val = p[1];
            if(key in table) {
                return ME.error(p);
            } 
            table[key] = val;
        }
        return ME.pure(astNode('object', table));
    }

    function myDefine(symbol, value) {
        if(typeof(symbol) !== 'string') {
            return ME.error('type error');
        }
        return ME.pure(astNode('define', {symbol: symbol, value: value}));
    }

    function mySetBang(symbol, value) {
        if(typeof(symbol) !== 'string') {
            return ME.error('type error');
        }
        return ME.pure(astNode('set!', {symbol: symbol, value: value}));
    }

    function myCond(branches, elseValue) {
        var i, br
        for(i = 0; i < branches.length; i++) {
            
        }
        return ME.pure(astNode('cond', {
            branches: branches, elseValue: elseValue
        });
    }

    function myLambda(params, bodies, returnValue) {
        var names = {};
        for(i = 0; i < params.length; i++) {
            if(typeof(s) !== 'string') {
                return ME.error("type error");
            }
            if(s.value in names) {
                return ME.error("value error -- duplicate parameter name");
            }
            names[s.value] = 1;
        });
        return ME.pure(astNode('lambda', {
            parameters: params, bodies: bodies,
            returnValue: returnValue
        }));
    }
    
    function wrap(f) {
        return function() {
            // extract the arguments to the inner function into an array
            var args = Array.prototype.slice.call(arguments, 0),
                // the metadata is the 1st arg
                meta = args.shift(),
                // now apply f to the rest of the args
                obj = f.apply(null, args);
            // 'obj' construction may have failed
            return obj.fmap(function(v) {
                // add in the meta-data if it didn't
                v.meta = meta;
                // ugh.  mutation
                return v;
            });
        };
    }
    

    return {
        'Number'     :  wrap(myNumber),
        'Char'       :  wrap(myChar),
        'Symbol'     :  wrap(mySymbol),
        'List'       :  wrap(myList),
        'Object'     :  wrap(myObject),
        'Application':  wrap(myApplication),
        'Define'     :  wrap(myDefine),
        'SetBang'    :  wrap(mySetBang),
        'Cond'       :  wrap(myCond),
        'Lambda'     :  wrap(myLambda)
    };

})(MaybeError);