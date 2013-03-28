define(["app/ast", "libs/parser", "libs/maybeerror"], function(AST, P, MaybeError) {
    "use strict";
        
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
    

    var pList = tokentype('open-square')
        .bind(function(open) {
            return pForm.many0()
                .fmap(function(xs) {return AST.list(xs, open.meta);})
                .seq2L(P.item.bind(function(close) {
                    if ( close.value === ']' ) {
                        return P.pure(null);
                    }
                    return P.error({'message': 'unable to match list', 'open': open, 'expected close': close});
                }))
                .commit({'message': 'unable to match list', 'open': open});
        });
    
    var pApplication = tokentype('open-paren')
        .bind(function(open) {
            return P.app(AST.application,
                         pForm,
                         pForm.many0(),
                         P.pure(open.meta))
                .seq2L(P.item.bind(function(close) {
                    if ( close.value === ')' ) {
                        return P.pure(null);
                    }
                    return P.error({'message': 'unable to match application', 'open': open, 'expected close': close});
                }))
                .commit({'message': 'unable to match application', 'open': open});
        });
    
    var bareSymbol = pSymbol.fmap(function(s) {
        return s.value;
    });
    
    function symbol(name) {
        return pSymbol.check(function(s) {return s.value === name;});
    }
    
    function error(rule, expected, open) {
        return {
            'rule'   : rule,
            'message': 'expected ' + expected,
            'open'   : open
        };
    }
    
    function pDefine(open) {
        return symbol('define')
            .seq2R(P.app(AST.define, 
                         bareSymbol.commit(error('define', 'symbol', open)),
                         pForm.commit(error('define', 'form', open)),
                         P.pure(open.meta)));
    }

    function pSet(open) {
        return symbol('set')
            .seq2R(P.app(AST.set, 
                         bareSymbol.commit(error('set', 'symbol', open)),
                         pForm.commit(error('set', 'form', open)),
                         P.pure(open.meta)));
    }
        
    var condBranches =
        tokentype('open-square')
        .seq2R(tokentype('open-square')
              .seq2R(P.all([pForm, pForm]))
              .seq2L(tokentype('close-square'))
              .many0())
        .seq2L(tokentype('close-square'));
    
    function pCond(open) {
        return symbol('cond')
            .seq2R(P.app(AST.cond, 
                         condBranches.commit(error('cond', 'predicate/result pairs', open)),
                         pForm.commit(error('cond', 'else form', open)),
                         P.pure(open.meta)));
    }
    
    function myLambda(_1, _2, params, _3, bodies, meta) {
        var lastBody = bodies.pop();
        return AST.lambda(params, bodies, lastBody, meta);
    }
    
    function uniqueNames(params) {
        var names = {}, i;
        for(i = 0; i < params.length; i++) {
            if(params[i] in names) {
                return P.error({'message': 'repeated symbol in parameter list', 
                                'rule': 'lambda', 'symbols': params});
            }
            names[params[i]] = 1;
        }
        return P.pure(params);
    }
    
    var onlySymbol = 
        bareSymbol.plus(P.item.bind(function(t) {
                                        if ( t.tokentype === 'close-square' ) {
                                            return P.zero;
                                        }
                                        return P.error({'message': 'non-symbol in parameter list',
                                                        'rule': 'lambda', token: t});
                                    }));
            
    function pLambda(open) {
        return P.app(
	        myLambda,
	        symbol('lambda'),
	        tokentype('open-square').commit(error('lambda', 'parameter list', open)),
	        onlySymbol.many0().bind(uniqueNames),
	        tokentype('close-square').commit(error('lambda', "']' to close parameter list", open)),
	        pForm.many1().commit(error('lambda', 'body forms', open)),
	        P.pure(open.meta));
	}
        
    var pSpec = tokentype('open-curly')
        .bind(function(open) {
            return P.any([pDefine(open), pSet(open), pCond(open), pLambda(open)])
                .commit({'message': 'unable to match special form', 'open': open})
                .seq2L(P.item.bind(function(close) {
                    if ( close.value === '}' ) {
                        return P.pure(null);
                    }
                    return P.error({'message': 'unable to match special form',
                                    'open': open, 'expected close': close});
                }));
        });
    
    // written this way to allow mutual recursion
    pForm.parse = P.any([pSpec, pApplication, pList, pSymbol, pNumber, pString]).parse;
    
    // probably want to check for unmatched close braces here
    var parser = pForm.many0();
    
    return {
        'number'       :  pNumber,
        'symbol'       :  pSymbol,
        'string'       :  pString,
        'application'  :  pApplication,
        'list'         :  pList,
        'special'      :  pSpec,
        'form'         :  pForm,
        'parse'        :  parser
    };

});
