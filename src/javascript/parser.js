var Parser = (function(P) {

    function myObject(pairs, line, column) {
        return {
            type     : 'objectliteral',
            entries  :  pairs,
            line     :  line,
            column   :  column
        };
    }
    
    function myList(elements, line, column) {
        return {
            type      :  'listliteral',
            elements  :  elements,
            line      :  line,
            column    :  column
        };
    }
    
    function myApp(op, args, line, column) {
        return {
            'type'     :  'application',
            'operator' :  op,
            'arguments':  args,
            line       :  line,
            column     :  column
        };
    }
    
    function mySpecial(op, args, line, column) {
        return {
            'type'     :  'special',
            'operator' :  op,
            'arguments':  args,
            line       :  line,
            column     :  column
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
    
    function myObject2(start) {
        return function(pairs) {
            return myObject(pairs, start.line, start.column);
        };
    }
    
    function myList2(start) {
        return function(elems) {
            return myList(elems, start.line, start.column);
        };
    };
    
    function mySpecial2(start) {
        return function(op, args) {
            return mySpecial(op, args, start.line, start.column);
        };
    }
    
    function myApp2(start) {
        return function(op, args) {
            return myApp(op, args, start.line, start.column);
        };
    }

    var pForm = P.Parser.error("need to set function");    
    
    var pObject = tokentype('open-curly').bind(
        function(t) {
            return P.Parser.all([pString, pForm])
                .many0()
                .fmap(myObject2(t))
                .seq2L(tokentype('close-curly'))
                .commit({line: t.line, column: t.column, rule: 'object literal'});
        });
    
    var pList = tokentype('open-square').bind(
        function(t) {
            return pForm 
                .many0()
                .fmap(myList2(t))
                .seq2L(tokentype('close-square'))
                .commit({line: t.line, column: t.column, rule: 'list literal'});
        });
    
    function whatEvs(start, middle, end, rule) {
        return start.bind(
            function(t) {
                return middle
                    .seq2L(end)
                    .fmap(function(a) {a.line = t.line; a.column = t.column; return a;})
                    .commit({line: t.line, column: t.column, rule: rule});
            });
    };
    
    var pApp = whatEvs(
        tokentype('open-paren'),
        P.Parser.app(myApp, pForm, pForm.many0()),
        tokentype('close-paren'),
        'application');
    
    var pApp = tokentype('open-paren').bind(
        function(t) {
            return P.Parser.app(
                    myApp2(t),
                    pForm,
                    pForm.many0())
                .seq2L(tokentype('close-paren'))
                .commit({line: t.line, column: t.column, rule: 'application'});
        });
    
    var pSpec = tokentype('open-special').bind(
        function(t) {
            return P.Parser.app(
                    mySpecial2(t),
                    pForm,
                    pForm.many0())
                .seq2L(tokentype('close-special'))
                .commit({line: t.line, column: t.column, rule: 'special-form application'});
        });
    
    // written as a function instead of a `var` to allow mutual recursion,
    //   and functions are hoisted (I believe)
    pForm.parse = P.Parser.any([pSpec, pApp, pList, pObject, pSymbol, pNumber, pString]).parse;
    // will this work?  if not, switch back to a function maybe
    
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