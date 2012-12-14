var Parser = (function(P) {

    function myObject(pairs) {
        return {
            'type': 'objectliteral',
            'entries': pairs
        };
    }
    
    function myList(elements) {
        return {
            'type': 'listliteral',
            'elements': elements
        };
    }
    
    function myApp(args) {
        return {
            'type'     : 'application',
            'operator' : args[0],
            'arguments': args[1]
        };
    }
    
    function mySpecial(args) {
        return {
            'type'     : 'special',
            'operator' : args[0],
            'arguments': args[1]
        };
    }


    // parsers
    
    function tokentype(type) {
        return P.satisfy(
            function(t) {
                return t.type === type;
            });
    }

    var pNumber = P.either(tokentype('integer'), tokentype('float'));
    
    var pString = tokentype('string');

    var pSymbol = tokentype('symbol');
    
    function openCommit(open, middle, close, message) {
        return P.seq2R(open, 
                       P.commit(P.seq2L(middle, 
                                        close),
                                message));
    }
    
    var pObject = openCommit(
        tokentype('open-curly'),
        P.fmap(myObject, P.many0(P.all([pString, pForm]))),
        tokentype('close-curly'), 
        'object');
    
    var pList = openCommit(
        tokentype('open-square'),
        P.fmap(myList, P.many0(pForm)),
        tokentype('close-square'),
        'list');
    
    var pApp = openCommit(
        tokentype('open-paren'),
        P.fmap(myApp, P.all([pForm, P.many0(pForm)])), // could just do P.many1(pForm), but this keeps 1st separate from rest
        tokentype('close-paren'),
        'application');
    
    var pSpec = openCommit(
        tokentype('open-special'),
        P.fmap(mySpecial, P.all([pSymbol, P.many0(pForm)])),
        tokentype('close-special'),
        'special-form application');

    // written as a function instead of a `var` to allow mutual recursion,
    //   and functions are hoisted (I believe)
    function pForm(xs) {
        return P.any([pSpec, pApp, pList, pObject, pSymbol, pNumber, pString])(xs);
    }
    
    var parser = P.many0(pForm);
    
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

})(ParserCombs);