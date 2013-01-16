var PParser = (function(AST, P, MaybeError) {
    "use strict";
    
    /* parser tasks:
      1. [Token] -> MaybeError AST
      2. accurate error reporting
      3. keep track of 'metadata' of astnodes
    */
    
    function tokentype(type) {
        return P.satisfy(
            function(t) {
                return t.tokentype === type;
            });
    }

    var pNumber = tokentype('integer')
        .plus(tokentype('float'))
        .fmap(function(t) {return AST.number(parseFloat(t.value), t.meta);});
    
    function stringAction(t) {
        var newList = t.value.split('').map(AST.char);
        return AST.list(newList, t.meta);
    }
    
    var pString = tokentype('string')
        .fmap(stringAction);

    var pSymbol = tokentype('symbol')
        .fmap(function(t) {return AST.symbol(t.value, t.meta);});

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
    
    var pObject = delimited(
        tokentype('open-curly'),
        P.all([pForm, pForm]).many0().fmap(AST.object),
        tokentype('close-curly'),
        'object literal');
    
    var pList = delimited(
        tokentype('open-square'),
        pForm.many0().fmap(AST.list),
        tokentype('close-square'),
        'list literal');
    
    var pApp = delimited(
        tokentype('open-paren'),
        P.app(AST.application, pForm, pForm.many0()),
        tokentype('close-paren'),
        'application');
    
    var bareSymbol = pSymbol.fmap(function(s) {
        return s.value;
    });
    
    var pDefine = pSymbol.check(function(s) {return s.value === 'define';})
        .seq2R(P.app(AST.define, bareSymbol, pForm)); // TODO needs to pull .value out of symbol

    var pSetBang = pSymbol.check(function(s) {return s.value === 'set!';})
        .seq2R(P.app(AST.setBang, bareSymbol, pForm));
        
    var condBranches =
        tokentype('open-square')
        .seq2R(tokentype('open-square').seq2R(P.all([pForm, pForm])).seq2L(tokentype('close-square')).many0())
        .seq2L(tokentype('close-square'));
    
    /* alternatives for comparison (intended to be equivalent)
    var condBranches2 =
        P.all([
            tokentype('open-square'),
            P.all([tokentype('open-square'), pForm, pForm, tokentype('close-square')]).many0(),
            tokentype('close-square')])
        .fmap(function(v) {
            return v[1].map(function(w) {return [w[1], w[2]];});
        });
    
    var condBranches3 = pList
        .check(function(es) {
            for(var i = 0; i < es.elements.length; i++) {
                if(es.elements[i].asttype !== 'listliteral') {
                    return false;
                }
            }
            return true;
        });
    */

    var pCond = pSymbol.check(function(s) {return s.value === 'cond';})
        .seq2R(P.app(AST.cond, condBranches, pForm));
    
    // ignore _'s -- is this bad js??
    function myLambda(_1, _2, params, _3, bodies) {
        var lastBody = bodies.pop();
        return AST.lambda(params.map(function(p) {return p.value;}), bodies, lastBody);
    }
    
    // true if no name appears more than once, false otherwise
    function uniqueNames(params) {
        var names = {}, i;
        for(i = 0; i < params.length; i++) {
            if(params[i].value in names) {
                return false;
            }
            names[params[i].value] = 1;
        }
        return true;
    }
            
    var pLambda = P.app(
        myLambda,
        pSymbol.check(function(s) {return s.value === 'lambda';}),
        tokentype('open-square'),
        pSymbol.many0().check(uniqueNames),
        tokentype('close-square'),
        pForm.many1());
        
    var pSpec = delimited(
        tokentype('open-special'),
        P.any([pDefine, pSetBang, pCond, pLambda]),
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
        'define'  :  pDefine,
        'setBang' :  pSetBang,
        'cond'    :  pCond,
        'lambda'  :  pLambda,
        'form'    :  pForm,
        'parse'   :  parser
    };

})(AST, ParserCombs, MaybeError);