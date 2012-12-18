var Parser = (function(P) {

    function myObject(pairs) {
        return {
            type     : 'objectliteral',
            entries  :  pairs
        };
    }
    
    function myList(elements) {
        return {
            type      :  'listliteral',
            elements  :  elements
        };
    }
    
    function myApp(op, args) {
        return {
            'type'     :  'application',
            'operator' :  op,
            'arguments':  args
        };
    }
    
    function mySpecial(op, args) {
        return {
            'type'     :  'special',
            'operator' :  op,
            'arguments':  args
        };
    }


    // parsers
    
    function tokentype(type) {
        return P.Parser.satisfy(
            function(t) {
                return t.type === type;
            });
    }

    var pNumber = tokentype('integer').plus(tokentype('float'));
    
    var pString = tokentype('string');

    var pSymbol = tokentype('symbol');

    var pForm = P.Parser.error("need to set function");    

    function delimited(start, middle, end, rule) {
        return start.bind(
            function(t) {
                return middle
                    .seq2L(end)
                    .fmap(function(a) {a.line = t.line; a.column = t.column; return a;})
                    .commit({line: t.line, column: t.column, rule: rule});
            });
    };
    
    var pObject = delimited(
        tokentype('open-curly'),
        P.Parser.all([pString, pForm]).many0().fmap(myObject),
        tokentype('close-curly'),
        'object literal');
    
    var pList = delimited(
        tokentype('open-square'),
        pForm.many0().fmap(myList),
        tokentype('close-square'),
        'list literal');
    
    var pApp = delimited(
        tokentype('open-paren'),
        P.Parser.app(myApp, pForm, pForm.many0()),
        tokentype('close-paren'),
        'application');
    
    var pSpec = delimited(
        tokentype('open-special'),
        P.Parser.app(mySpecial, pForm, pForm.many0()),
        tokentype('close-special'),
        'special-form application');
    
    // written this way to allow mutual recursion
    pForm.parse = P.Parser.any([pSpec, pApp, pList, pObject, pSymbol, pNumber, pString]).parse;
    
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

})(ParserCombs);