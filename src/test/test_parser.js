function testParser(parse, tokens, testHelper) {

    module("AST construction");

    var tok = tokens.Token,
        astn = parse.ASTNode,
        app = function(op, args) {return astn('application', {'operator': op, 'arguments': args});},
        list = function(x) {return astn('list', x);},
        ch  = function(x) {return astn('char', x);},
        sym = function(x) {return astn('symbol', x);},
        num = function(x) {return astn('number', x);},
        str = function(x) {return astn('list', parse.expandString(x));},
        expExc = testHelper.expectException,
        op = tok('open-paren', '('),
        os = tok('open-square', '['),
        cp = tok('close-paren', ')'),
        cs = tok('close-square', ']');

    
    test("strings", function() {
        var str1 = tok('string', "yes"),
            emp = tok('string', "");
    
        deepEqual(
            {rest: [], result: str('yes')},
            parse.getNextForm([str1]), 
            'string tokens become AST lists of chars'
        );
        
        deepEqual({rest: [], result: list([])}, parse.getNextForm([emp]), "empty string token => empty AST list");
    });
    

    test("applications", function () {
        expect(4);

        var list1 = [
                op,
                tok('symbol', "+"),
                tok('string', 'str1'),
                tok('integer', "345"),
                cp
            ],
            list2 = [
                op,
                tok('symbol', "+"),
                op,
                tok('symbol', "-"),
                tok('string', ""),
                cp,
                tok('symbol', '>>>'),
                cp
            ];


        deepEqual(
            {'rest': [], 'result': app(sym('+'), [str('str1'), num(345)])},
            parse.getNextForm(list1), 
            "an 'Application' is delimited by parentheses ..."
        );

        deepEqual(
            {'rest': [], result: app(sym('+'), 
                                     [app(sym('-'), 
                                          [str("")]),
                                      sym('>>>')])},
            parse.getNextForm(list2),
            '... and may be nested'
        );
        
        expExc(function() {
            parse.getNextForm([op, cp]);
        }, 'ParseError', 'Application needs a function or special form -- cannot be empty');
    });

    
    test("getAtom", function() {
        var barf = tok('symbol', 'barf'),
            testCases = [
                ["'(' is not an atom",        [op, barf],                   false],
                [" ... nor is ')' ...",       [close, barf, op],            false],
                [" ... nor is a comment ...", [tok('comment', 'blargh')],   false],
                [" ... nor is whitespace",    [tok('whitespace', '\n \t')], false],
                ["symbols ARE atoms ...",     [barf, op, barf],             {'result': sym("barf"), 'rest': [tok('open-paren', '('), barf]}],
                ["... as are strings",        [tok('string', 'me!')],       {'result': str('me!'),  'rest': []}],
                ["... and integers",          [tok('integer', '145')],      {'result': num(145),    'rest': []}],
                ["... and floats",            [tok('float', '7.23')],       {'result': num(7.23),   'rest': []}],
                ["no atoms in empty list of tokens", [],                    false]
            ];
        
        testCases.map(function(data) {
            deepEqual(data[2], parse.getAtom(data[1]), data[0]);
        });
    });



    test("getList", function() {
        expect(15);

        var abc = tok('symbol', 'abc'),
            bleh = tok('string', 'bleh'),
            t6 = [os, abc, os, abc, cs, abc, cs, os, abc],
            t7 = [os, abc, os, abc, cs, abc],
            t8 = [os, os, os, os, cs, cs, cs, cs, abc],
            t9 = [os, os, os, os, cs, cs, cs],
            ll = Data.ListLiteral;


        var t = parse.getList([]);
        equal(false, t, "trying to get a list from an empty token stream returns false ... ");

        expExc(function() {
            parse.getList([os]);
        }, 'ParseError', "... while a '[' without a matching ']' throws an error,");

        expExc(function() {
            parse.getList([cs]);
        }, 'ParseError', "and a leading ']' also throws an error");

        var p = parse.getList([os, cs]);
        deepEqual({
            result: list([]),
            rest: []
        }, p, "[] is parsed as an empty list");

        var s = parse.getList([abc]);
        equal(false, s, "you can't get a list from a symbol ...");
      
        equal(false, parse.getList([str, sym]), " ... or from a string");

        var q = parse.getList([os, abc, cs, abc]);
        deepEqual({
            'result': list([sym('abc')]), 
            'rest':  [abc]
        }, q, "a list extends from the opening '[' to the next ']', unless ...");

        var r = parse.getList(t6);
        deepEqual({
            'result': list([
                sym('abc'),
                list([sym('abc')]),
                sym('abc')
            ]),
            'rest': t6.slice(7)
        }, r, "... it has a nested list, in which case it extends to its 'balancing' ']'");

        expExc(function() {
            parse.getList(t7);
        }, 'ParseError', "therefore: a missing 'balancing' ']' throws an error");
      
        var v = parse.getList(t8);
        deepEqual({
            'result': list([
                list([
                    list([
                        list([])])])]),
            'rest': t8.slice(8)
        }, v, "lists may be arbitrarily deeply nested ...");
      
        expExc(function() {
            parse.getList(t9);
        }, 'ParseError', "... as long as the parentheses match");
    });
    
    
    test("makeAST", function() {
    	var myTokens = [op, tok('symbol', '+'), os, tok('integer', '14'), tok('float', '23.2'), cs, tok('string', 'yes'), cp],
    	    myRes = [app(sym('+'), [
    	                       list([num(14), num(23.2)]),
    	                       str('yes')])];
    	
    	deepEqual(myRes,
    	    parse.makeAST(myTokens),
    	    'to sum it up:  the AST contains nodes of applications, lists, numbers, symbols, and chars'
    	);
    });

}