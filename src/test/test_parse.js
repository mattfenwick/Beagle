
function testParse(lang, testHelper) {

    module("tokenizer");
    var expectExc = testHelper.expectException;
    
    test("nextToken", function() {
        expect(14);
        
        var tok = lang.Token,
            testCases = [
                ['empty string', "",          false                                                 ],
                
                ['open-paren',   "((duh",     {'rest': '(duh',   'token': tok('open-paren', '(')   }],//change token name
                
                ['close-paren',  ") bleh",    {'rest': ' bleh',  'token': tok('close-paren', ')')  }],// ... here too
                
                ['symbol',       "abc)(()",   {'rest': ')(()',   'token': tok('symbol', 'abc')     }],
                
                ['number',       "12345 )",   {'rest': ' )',     'token': tok('symbol', '12345')   }],
                
                ['whitespace',   '\n\them\n', {'rest': 'hem\n',  'token': tok('whitespace', '\n\t')}],
                
                ["comments begin with a ; and end at the next newline (\\n)",      
                                 "; out\nme", {'rest': '\nme',   'token': tok('comment', ' out')   }],
                                 
                ['string',       '"abc" ',    {'rest': ' ',      'token': tok('string', 'abc')     }],
                
                ['open-square',  "[bl ; h",   {'rest': 'bl ; h', 'token': tok('open-square', '[')  }],
                
                ['close-square', "][][",      {'rest': '[][',    'token': tok('close-square', ']') }],
                
                ['string with whitespace',
                                 '"ab cd" f', {'rest': ' f',     'token': tok('string', 'ab cd')   }],
                                 
                ["symbols may not include ;'s",
                                 'wh;at',     {'rest': ';at',    'token': tok('symbol', 'wh')      }]
        ];
        
        testCases.map(function(data) {
            deepEqual(data[2], lang.nextToken(data[1]), data[0]);
        });
      
        expectExc(function() {
            lang.nextToken('"abc');
        }, 'ParseError', 'a " (start string) without a matching " (close string) throws an exception');
    });
    

    test("tokenize", function() {
        var tok = lang.Token,
            op = tok('open-paren', "("),
            cp = tok('close-paren', ')'),
            o = tok('symbol', '1'),
            p = tok('symbol', '+'),
            s = tok('whitespace', ' ');
        
        var testCases = [
            ["all consecutive whitespace is collapsed into a single token",
                         "  \t\n \t(abc)",       [tok('whitespace', "  \t\n \t"), op, tok('symbol', "abc"), cp]],
            ["adjacent '('s are separate tokens",
                         "(((((((\n  ",          [op, op, op, op, op, op, op, tok('whitespace', "\n  ")]],
            ["lots of ')'s, '('s, and whitespace",
                         "))) \t)\n(((",         [cp, cp, cp, tok('whitespace', " \t"), cp, tok('whitespace', "\n"), op, op, op]],
            ["symbols are terminated by '(' and ')'",
                         "abc123    abc(der)",   [tok('symbol', "abc123"), tok('whitespace', "    "), 
                                                  tok('symbol', "abc"), op, tok('symbol', "der"), cp]],
            ["lots of nested parentheses",
                         "(+ 1 1 (+ 1 1 (+ 1 (+ 1 1))))",
                                                 [op, p, s, o, s, o, s, op, p, s, o, s, o, s, 
                                                  op, p, s, o, s, op, p, s, o, s, o, cp, cp, cp, cp]],
            ["an empty string yields an empty list of tokens", 
                         "",                     []]
        ];
        
        testCases.map(function(data) {
            deepEqual(data[2], lang.tokenize(data[1]), data[0]);
        });
    });
    

    test("strip comments and whitespace", function() {
        var t1 = [
            lang.Token('comment', 'abc'), 
            lang.Token('string', 'derrrr'), 
            lang.Token('whitespace', '\t\n     \n'), 
            lang.Token('open-paren', '('),
            lang.Token('whitespace', '   \t\t\t\n\t')
        ]; 

        var t2 = lang.stripTokens(t1);

        deepEqual([t1[1], t1[3]], t2, 'all whitespace and comment tokens are discarded by stripping');
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
    
    

    test("check token separation", function() {

        var tokens = [lang.Token('symbol', 'abc'), lang.Token('comment', 'nope'),
                      lang.Token('whitespace', '   '), lang.Token('open-paren', '('),
                      lang.Token('close-paren', ')'), lang.Token('string', 'hahaha')];

        var types = {'string': 1, 'symbol': 1};

        var i, j, passed, myTokens;
        for(i = 0; i < tokens.length; i++) {
            for(j = 0; j < tokens.length; j++) {
                passed = true;
                try {
                    myTokens = [tokens[i], tokens[j]];
                    lang.checkTokenSeparation(myTokens);
                    passed = true;
                } catch(e) {
                    passed = false;
                };
                if( types[tokens[i].type] && types[tokens[j].type] ) {
                    ok(!passed, 'consecutive strings/symbols throws an exception');
                } else {
                    ok(passed, "no problem for tokens " + JSON.stringify(myTokens));
                }
            } // inner for-loop
        } // outer for-loop
    });
}
