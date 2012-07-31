function testReify(reify, data, parse, testHelper) {

    module("AST construction");

    var tok = parse.Token,
        app = data.Application,
        list = data.List,
        ch = data.Char,
        sym = data.Symbol,
        num = data.Number,
        str = data.makeCharList,
        expExc = testHelper.expectException;

    
    test("strings", function() {
        var str1 = tok('string', "yes"),
            emp = tok('string', "");
    
        deepEqual(
            {rest: [], result: str('yes')},
            reify.getForm([str1]), 
            'string tokens are reified into lists of chars'
        );
        
        deepEqual({rest: [], result: list([])}, reify.getForm([emp]), "empty string token => empty Lisp list");
    });
    
    
    test("symbols", function() {
    	ok(false, "THESE TESTS NEED TO BE MOVED TO THE TOKENIZATION MODULE");
    	
        var empty = tok('symbol', ""),
            starts = ['!', '@', '#', '$', '%', '^', '&', '*', '_', '-', '+', '=', '<', '>', '?', '/', 'a', 'z', 'A', 'Z'],
            // this is not an exhaustive list ... should it be?
            nonos = [',', '(', ')', '"', "'", '[', '{', '|', '\\'];
        
        starts.map(function(c) {
            deepEqual(
                {rest: [], result: sym(c)}, 
                reify.getForm([tok('symbol', c)]), 
                "Beagle symbols may start with " + c
            );
        });
        
        nonos.map(function(c) {
            expExc(function() {
                reify.getForm([tok('symbol', c)]);
            }, 'ValueError', 'Beagle symbols may *not* start with ' + c);
        });
        
        deepEqual(
            sym('j3451kl!@#$%^&*_-+=<>?/'),
            reify.getForm(SExpr('symbol', 'j3451kl!@#$%^&*_-+=<>?/')),
            "Beagle symbols may start with a letter or !@#$%^&*_-+=<>?/, followed by any number of letters, digits or !@#$%^&*()_-+=<>?/"
        );

        expExc(function() {
            reify.getForm(empty);
        }, 'ValueError', "empty symbols can't be reified");
    });
    

    test("applications", function () {
        expect(6);

        var list1 = SExpr('list', [
                SExpr('symbol', "+"),
                SExpr('string', 'str1'),
                SExpr('symbol', "345")
            ]),
            list2 = SExpr('list', [
                SExpr('symbol', "+"),
                SExpr('list', [SExpr('symbol', "-"), SExpr('string', "")]),
                SExpr('symbol', '>>>')
            ]),
            badtype = SExpr('boolean', 'false');


        var l1 = app([sym('+'), str('str1'), num(345)]);
        deepEqual(
            l1, 
            reify.getForm(list1), 
            'lists containing symbols and strings are easy to reify ...'
        );

        var l2 = app(
            [sym('+'), app([sym('-'), str("")]), sym('>>>')]
        );
        deepEqual(
            l2, 
            reify.getForm(list2),
            '... as are nested lists (as long as they have strings and symbols!)'
        );

        expExc(function() {
            reify.getForm(badtype);
        }, 'TypeError', "reification only recognizes SExpression types 'string', 'symbol', and 'list'");
        
        expExc(function() {
            reify.getForm(SExpr('list', []));
        }, 'SyntaxError', 'the empty list cannot be reified, because an Application needs a list containing (at least) a function or special form');
    });


    test("number reification", function () {
        expect(14);

        var int_ = SExpr('symbol', "345"),
            float1 = SExpr('symbol', "03."),
            float2 = SExpr('symbol', "3.456"),
            float3 = SExpr('symbol', ".001"),
            float4 = SExpr('symbol', "0.00"),
            dec = SExpr('symbol', '.'),
            b1 = SExpr('symbol', "true"),
            str1 = SExpr('string', '1234'),
            notANum = SExpr('symbol', '4..0');

        deepEqual(
          data.Number(345), 
          reify.makePrimitives(int_),
          'reification produces a number if the symbol is all digits ...'
        );

        deepEqual(
          data.Number(3), 
          reify.makePrimitives(float1), 
          "... if it's digits followed by a decimal point ..."
        );

        deepEqual(
          data.Number(3.456), 
          reify.makePrimitives(float2), 
          "... or digits, decimal point, and more digits ..."
        );

        deepEqual(
          data.Number(0.001), 
          reify.makePrimitives(float3), 
          "... a leading decimal point followed by digits is fine ..."
        );

        deepEqual(
          data.Number(0.00), 
          reify.makePrimitives(float4), 
          "... and it's also okay to have leading and trialing 0's"
        );
        
        expExc(function() {
            reify.makePrimitives(notANum);
        }, 'ValueError', "that if it starts with a digit, it *must* be reifiable as a number");

        deepEqual(
          data.Symbol('true'), 
          reify.makePrimitives(b1),
          'that numbers are the only symbols that get special treatment from reification,'
        );

        deepEqual(
          str('1234'),
          reify.makePrimitives(str1),
          "that only sexpressions whose types are 'symbol' -- not 'string' -- can be reified into numbers,"
        );
        
        expExc(function() {
            reify.makePrimitives(SExpr('symbol', '.'));
        }, 'ValueError', "that the sexpression symbol '.' can not be reified,");

        deepEqual(
            num(0),
            reify.makePrimitives(SExpr('symbol', '0')),
            "that sexpression symbols with leading digits must be reified as numbers,"
        );
        
        expExc(function() {
            reify.makePrimitives(SExpr('symbol', '0a'));
        }, 'ValueError', "and thus, if it starts with a digit, but can't be reified as a number, it's an error");
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