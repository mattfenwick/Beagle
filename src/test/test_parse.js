
function testParse(lang, testHelper) {

    module("tokenizer");
    var expectExc = testHelper.expectException,
        tok = lang.Token;
    
    test("nextToken", function() {
        expect(15);
        
        var testCases = [
                ['empty string', "",          false                                                 ],
                
                ['open-paren',   "((duh",     {'rest': '(duh',   'token': tok('open-paren', '(')   }],//change token name
                
                ['close-paren',  ") bleh",    {'rest': ' bleh',  'token': tok('close-paren', ')')  }],// ... here too
                
                ['symbol',       "abc)(()",   {'rest': ')(()',   'token': tok('symbol', 'abc')     }],
                
                ['integer',      "12345 )",   {'rest': ' )',     'token': tok('integer', '12345')  }],
                
                ['float',        ".23 bleh",  {'rest': ' bleh',  'token': tok('float', ".23")      }],
                
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
        }, 'TokenError', 'a " (start string) without a matching " (close string) throws an exception');
    });
    
    
    test("symbols", function() {        
        var starts = ['!', '@', '#', '$', '%', '^', '&', '*', '_', '-', '+', '=', '<', '>', '?', '/', 'a', 'z', 'A', 'Z'],
            // this is not an exhaustive list ... should it be?
            nonos = [',', '"', "'", '{', '}', '|', '\\'];
        
        starts.map(function(c) {
            deepEqual(
                {rest: "", token: tok('symbol', c)}, 
                lang.nextToken(c), 
                "Beagle symbols may start with " + c
            );
        });
        
        nonos.map(function(c) {
            expectExc(function() {
                lang.nextToken(c);
            }, 'TokenError', 'Beagle symbols may *not* start with ' + c);
        });
        
        deepEqual(
            {'rest': '', 'token': tok('symbol', 'j3451kl!@#$%^&*_-+=<>?/')},
            lang.nextToken('j3451kl!@#$%^&*_-+=<>?/'),
            "Beagle symbols may start with a letter or !@#$%^&*_-+=<>?/, followed by any number of letters, digits or !@#$%^&*()_-+=<>?/"
        );
    });


    test("numbers", function () {
        var testCases = [
                ['a number if the symbol is all digits',     '345',   tok('integer', '345')],
                ['digits followed by a decimal point',       '03.',   tok('float', '03.'  )],
                ['digits, decimal point, and more digits',   '3.456', tok('float', '3.456')],
                ['leading decimal point followed by digits', '.001',  tok('float', '.001' )],
                ["leading and trialing 0's",                 '0.00',  tok('float', '0.00' )]];

        testCases.map(function(data) {
            deepEqual({"rest": "", "token": data[2]}, lang.nextToken(data[1]), data[0]);
        });
        
        expectExc(function() {
            lang.nextToken('4..0');
        }, 'TokenError', "if it starts with a digit, it *must* be tokenizable as an integer or float");

        expectExc(function(){
            lang.nextToken('0a');
        }, 'TokenError', "another example of forcing number tokenization");
        
        expectExc(function() {
            lang.nextToken('.');
        }, 'TokenError', "'.' is not a valid token");
    }); 
    

    test("tokenize", function() {
        var tok = lang.Token,
            op = tok('open-paren', "("),
            cp = tok('close-paren', ')'),
            o = tok('integer', '1'),
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
    
    

    test("check token separation", function() {

        var tokens = [lang.Token('symbol',      'abc'),   lang.Token('comment',    'nope'),
                      lang.Token('whitespace',  '   '),   lang.Token('open-paren', '('),
                      lang.Token('close-paren', ')'),     lang.Token('string',     'hahaha'),
                      lang.Token('integer',     '32'),    lang.Token('float',      '3.45')];

        var types = {'string': 1, 'symbol': 1, 'integer': 1, 'float': 1};

        var passed, myTokens;
        tokens.map(function(t1) {
        	tokens.map(function(t2) {
                passed = true;
                try {
                    myTokens = [t1, t2];
                    lang.checkTokenSeparation(myTokens);
                    passed = true;
                } catch(e) {
                    passed = false;
                };
                if( types[t1.type] && types[t2.type] ) {
                    ok(!passed, "consecutive " + JSON.stringify(myTokens) + ' should throw an exception');
                } else {
                    ok(passed, "consecutive " + JSON.stringify(myTokens) + ' should NOT throw');
                }
            }); // inner
        }); // outer
    });
}
