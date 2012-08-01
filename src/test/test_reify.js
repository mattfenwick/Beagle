function testReify(reify, data, parse, testHelper) {

    module("AST construction");

    var tok = parse.Token,
        app = data.Application,
        list = data.List,
        ch = data.Char,
        sym = data.Symbol,
        num = data.Number,
        str = data.makeCharList,
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
            reify.getNextForm([str1]), 
            'string tokens are reified into lists of chars'
        );
        
        deepEqual({rest: [], result: list([])}, reify.getNextForm([emp]), "empty string token => empty Lisp list");
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
            reify.getNextForm(list1), 
            "an 'Application' is delimited by parentheses ..."
        );

        deepEqual(
            {'rest': [], result: app(sym('+'), 
            		                 [app(sym('-'), 
            		                	  [str("")]),
            		                  sym('>>>')])},
            reify.getNextForm(list2),
            '... and may be nested'
        );
        
        expExc(function() {
            reify.getNextForm([op, cp]);
        }, 'ValueError', 'Application needs (at least) a function or special form -- an empty one cannot be reified');
    });
      

    module("parse");
    
    test("getAtom", function() {
        var open = lang.Token('open-paren', '('),
            close = lang.Token('close-paren', ')'),
            barf = lang.Token('symbol', 'barf'),
            testCases = [
                ["'(' is not an atom",        [open, lang.Token('symbol', "duh")], false],
                [" ... nor is ')' ...",       [close, "abc", open],                false],
                [" ... nor is a comment ...", [lang.Token('comment', 'blargh')],   false],
                [" ... nor is whitespace",    [lang.Token('whitespace', '\n \t')], false],
                ["symbols ARE atoms ...",     [lang.Token('symbol', "123abc"), open, barf],
                          {'result': lang.SExpression('symbol', "123abc"),'rest': [open, barf]}],
                ["... as are strings",        [lang.Token('string', 'me!')], 
                          {'result': lang.SExpression('string', 'me!'), 'rest': []}],
                ["and don't forget:  there's no atoms in an empty list of tokens", [], false]
            ];
        
        testCases.map(function(data) {
            deepEqual(data[2], lang.getAtom(data[1]), data[0]);
        });
    });



    test("getList", function() {
        expect(11);

        var open = lang.Token('open-paren', '('),
            close = lang.Token('close-paren', ')'),
            sym = lang.Token('symbol', 'abc'),
            str = lang.Token('string', 'bleh');

        var t0 = [],
            t1 = [open],
            t2 = [close],
            t3 = [open, close],
            t4 = [sym],
            t5 = [open, sym, close, sym],
            t6 = [open, sym, open, sym, close, sym, close, open, sym],
            t7 = [open, sym, open, sym, close, sym],
            t8 = [open, open, open, open, close, close, close, close, sym],
            t9 = [open, open, open, open, close, close, close];


        var t = lang.getList(t0);
        equal(false, t, "trying to get a list from an empty token stream returns false ... ");

        var a = true;
        try {
            lang.getList(t1);
            a = false;
        } catch(e) {}
        ok(a, "... while a '(' without a matching ')' throws an error,");

        var b = true;
        try {
            lang.getList(t2);
            b = false;
        } catch(e) {}
        ok(b, "and a leading ')' also throws an error");

        var p = lang.getList(t3);
        deepEqual({
            result: lang.SExpression('list', []),
            rest: []
        }, p, "() is parsed as an empty list");

        var s = lang.getList(t4);
        equal(false, s, "you can't get a list from a symbol ...");
      
        equal(false, lang.getList([str, sym]), " ... or from a string");

        var q = lang.getList(t5);
        deepEqual({
            'result': lang.SExpression('list', [lang.SExpression('symbol', 'abc')]), 
            'rest':  t5.slice(3)
        }, q, "a list extends from the opening '(' to the next ')', unless ...");

        var r = lang.getList(t6);
        deepEqual({
            'result': lang.SExpression('list', [
                lang.SExpression('symbol', 'abc'),
                lang.SExpression('list', [lang.SExpression('symbol', 'abc')]),
                lang.SExpression('symbol', 'abc')
            ]),
            'rest': t6.slice(7)
        }, r, "... it has a nested list, in which case it extends to its 'balancing' ')'");

        var u = true;
        try {
            lang.getList(t7);
            u = false;
        } catch(e) {};
        ok(u, "therefore: a missing 'balancing' ')' throws an error");
      
        var v = lang.getList(t8);
        deepEqual({
            'result': lang.SExpression('list',
                [lang.SExpression('list',
                    [lang.SExpression('list',
                        [lang.SExpression('list', [])])])]),
            'rest': t8.slice(8)
        }, v, "lists may be arbitrarily deeply nested ...");
      
        var w = true;
        try {
            lang.getList(t9);
            w = false;
        } catch(e) {};
        ok(w, "... as long as the parentheses match");
    });

}