var ASTBuilder = (function(AST, MaybeError) {

    /* responsibilities:
      1. ParseTree -> MaybeError AST
      2. accurate error reporting
      3. keep track of meta-data of ast nodes
    */
    
    var pure = MaybeError.pure,
        error = MaybeError.error;
    
    function buildNumber(num) {
        return pure(AST.number(num.value));
    }
    
    function buildString(str) {
        return pure(AST.list(str.value.split('').map(AST.char)));
    }

    function buildSymbol(sym) {
        return pure(AST.symbol(sym.value));
    }
    
    // [MaybeError a] -> MaybeError [a]
    function commute(items) {
        var i,
            cleaned = [];
        for(i = 0; i < items.length; i++) {
            if(items[i].status === 'success') {
                cleaned.push(items[i].value);
            } else {
                return items[i];
            }
        }
        return pure(cleaned);
    }
    
    function buildApplication(app) {
        var op = build(app.operator),
            args = commute(app.arguments.map(build));
        if(op.status !== 'success') {
            return op;
        } else if(args.status !== 'success') {
            return args;
        }
        return pure(AST.application(op.value, args.value));
    }
    
    function buildList(list) { // TODO unchecked ... obviously
        return commute(list.elements.map(build))
            .fmap(function(es) {
                return AST.list(es);
            });
    }
    
    function buildObject(obj) {
        var i, pair, key, val;
        for(i = 0; i < obj.elements.length; i++) {
            pair = obj.elements[i];
            key = pair[0];
            val = build(pair[1]);
            if(val.status !== 'success') {
                return val;
            }
            table[key] = val;
        }
        return pure(AST.object(table));
    }
    
    function buildLambda() {
    
    }
    
    // 1. 2 args
    // 2a. build branches
    // 2b. build else-val
    // 2. 1st arg is list
    // 3. of lists
    // 4. of length 2
    function buildCond(node) { // TODO clean up these 30 lines of pure, unadulterated ugliness
        if(node.arguments.length !== 2) {
            return error('cond needs 2 arguments');
        }
        var branches = build(node.arguments[0]),
            elseValue = build(node.arguments[1]),
            bC, evC, pair, i, brs;
        if(branches.status !== 'success') {
            return branches;
        }
        if(elseValue.status !== 'success') {
            return elseValue;
        }
        bC = branches.value;
        evC = elseValue.value;
        if(bC.asttype !== 'listliteral') {
            return error('1st arg to cond must be list');
        }
        for(i = 0; i < bC.elements.length; i++) {
            pair = bC.elements[i];
            if(pair.asttype !== 'listliteral') {
                return error('if/else branches must be lists of length 2');
            }
            if(pair.elements.length !== 2) {
                return error('if/else branches must be lists of length 2');
            }
            brs.push(pair.elements);// strip out the rest, leaving just the js arrays, right?
        }
        return pure(AST.cond(brs, evC));
    }
    
    function buildDefine() {
        throw new Error('oops');
    }
    
    // error possibilities:
    //  1. more/less than 2 arguments
    //  2. build-ing the operator returns an error
    //  3. build-ing the value returns an error
    //  4. operator is not a symbol
    function buildSetBang(node) {
        if(node.arguments.length !== 2) {
            return error('set! needs 2 arguments, got ' + node.arguments.length);
        }
        var sym = build(node.arguments[0]),
            val = build(node.arguments[1]),
            symC, valC; // 'symbol Cleaned', 'value Cleaned' 
        if(sym.status !== 'success') {
            return sym;
        }
        symC = sym.value;
        if(val.status !== 'success') {
            return val;
        }
        valC = val.value;
        if(symC.asttype !== 'symbol') {
            return error("set!'s 1st argument must be a symbol");
        }
        return pure(AST.setBang(symC.value, valC));
    }
    
    var SPECIALS = {
        'lambda' :  buildLambda,
        'define' :  buildDefine,
        'set!'   :  buildSetBang,
        'cond'   :  buildCond
    };
    
    function buildSpecial(node) {
        if(!(node.operator in SPECIALS)) {
            return error('unrecognized special form -- ' + node.operator); // TODO what data belongs in this error?  what about the metadata?
        }
        return SPECIALS[node.operator](node);
    }
    
    var ACTIONS = {
        'number'  :  buildNumber,
        'string'  :  buildString,
        'symbol'  :  buildSymbol,
        'application':  buildApplication,
        'list'    :  buildList,
        'object'  :  buildObject,
        'special' :  buildSpecial
    };
    
    function build(node) {
        if(!(node.nodetype in ACTIONS)) {
            throw new Error('unrecognized parsetree node type -- ' + node.nodetype);
        }
        var astNode = ACTIONS[node.nodetype](node);
        return astNode.fmap(function(a) { // TODO not really sure if this is right; just kind of hacked it together
            a.meta = node.meta;
            return a; // the intent here is to copy the metadata just once in this source file to avoid having to do it in each function
        });
    }
    

    return {
        build            :  build
    };

})(AST, MaybeError);