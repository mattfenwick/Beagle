var PParser = (function(ParseTree, P, MaybeError) {
    "use strict";
    
    /* parser tasks:
      1. [Token] -> MaybeError ParseTree
      2. accurate error reporting
      3. keep track of 'metadata' of parsenodes
    */
    
    function tokentype(type) {
        return P.satisfy(
            function(t) {
                return t.tokentype === type;
            });
    }

    var pNumber = tokentype('integer')
        .plus(tokentype('float'))
        .fmap(function(t) {return ParseTree.number(parseFloat(t.value), t.meta);});
    
    var pString = tokentype('string')
        .fmap(function(t) {return ParseTree.string(t.value, t.meta);});

    var pSymbol = tokentype('symbol')
        .fmap(function(t) {return ParseTree.symbol(t.value, t.meta);});

    var pForm = P.error("javascript hack to allow mutual recursion -- still need to set function");    

    // return value's meta is the meta of the 'start' parser
    function delimited(start, middle, end, rule) {
        return start.bind(
            function(t) {
                return middle
                    .seq2L(end)
                    .fmap(function(a) {a.meta = t.meta; return a;}) // oops, mutation.  sorry
                    .commit({meta: t.meta, rule: rule});
            });
    };
    
    // [(string, form)] -> Parser t Object ... etc.
    function myObject(pairs) {
        return new P(function(xs) {
            var table = {};
            for(var i = 0; i < pairs.length; i++) {
                if(pairs[i][0].value in table) {
                    return MaybeError.error('duplicate key ' + pairs[i][0].value);
                }
                table[pairs[i][0].value] = pairs[i][1];
            }
            return MaybeError.pure({result: ParseTree.object(table), rest: xs});
        });
    }
    
    var pObject = delimited(
        tokentype('open-curly'),
        P.all([pString, pForm]).many0().bind(myObject),
        tokentype('close-curly'),
        'object literal');
    
    var pList = delimited(
        tokentype('open-square'),
        pForm.many0().fmap(ParseTree.list),
        tokentype('close-square'),
        'list literal');
    
    var pApp = delimited(
        tokentype('open-paren'),
        P.app(ParseTree.app, pForm, pForm.many0()),
        tokentype('close-paren'),
        'application');
    
    function mySpecial(op, args) {
        return ParseTree.special(op.value, args);
    }
    
    var pSpec = delimited(
        tokentype('open-special'),
        P.app(mySpecial, pForm, pForm.many0()),
        tokentype('close-special'),
        'special-form application');
    
    // written this way to allow mutual recursion
    pForm.parse = P.any([pSpec, pApp, pList, pObject, pSymbol, pNumber, pString]).parse;
    
    var parser = pForm.many0();
    
    return {
        'number'  :  pNumber,
        'symbol'  :  pSymbol,
        'string'  :  pString,
        'app'     :  pApp,
        'list'    :  pList,
        'object'  :  pObject,
        'special' :  pSpec,
        'form'    :  pForm,
        'parse'   :  parser
    };

})(ParseTree, ParserCombs, MaybeError);