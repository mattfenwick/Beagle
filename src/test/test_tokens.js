
function testTokens(tokenizer, testHelper) {

    module("tokenizer");
    var expectExc = testHelper.expectException,
        tok = tokenizer.Token;
    
    test("nextToken", function() {
        expect(17);
        
        function result(rest, type, match, newLine, newCol) {
            return {
                column :  newCol,
                line   :  newLine,
                rest   :  rest, 
                status :  'success',
                token  :  tok(type, match, 1, 1)
            };
        }
        
        var testCases = [
                ['empty string', ""     ,       false                                       ],
                
                ['open-paren',   "((duh",       result('(duh', 'open-paren', '(', 1, 2)     ],
                
                ['close-paren',  ") bleh",      result(' bleh', 'close-paren', ')', 1, 2)   ],
                
                ['symbol',       "abc)(()",     result(')(()', 'symbol', 'abc', 1, 4)       ],
                
                ['integer',      "12345 )",     result(' )', 'integer', '12345', 1, 6)      ],
                
                ['float',        ".23 bleh",    result(' bleh', 'float', ".23", 1, 4)       ],
                
                ['whitespace',   '\n\them\n',   result('hem\n', 'whitespace', '\n\t', 2, 2) ],
                
                ["comments begin with a ; and end at the next newline (\\n)",      
                                 "; out\nme",   result('\nme', 'comment', ' out', 1, 6)     ],
                                 
                ['string',       '"abc" ',      result(' ', 'string', 'abc', 1, 6)          ],
                
                ['open-square',  "[bl ; h",     result('bl ; h', 'open-square', '[', 1, 2)  ],
                
                ['close-square', "][][",        result('[][', 'close-square', ']', 1, 2)    ],
                
                ['open-curly',   "{me ; h",     result('me ; h', 'open-curly', '{', 1, 2)   ],
                
                ['close-curly',  "} 11[][",     result(' 11[][', 'close-curly', '}', 1, 2)  ],
                
                ['string with whitespace',
                                 '"ab cd" f',   result(' f', 'string', 'ab cd', 1, 8)       ],
                                 
                ["symbols may not include ;'s",
                                 'wh;at',       result(';at', 'symbol', 'wh', 1, 3)         ],

                ["open-special", ",(abc",       result('abc', 'open-special', ',(', 1, 3)   ],

                ["close-special",",))[]",       result(')[]', 'close-special', ',)', 1, 3)  ]
        ];
        
        testCases.map(function(data) {
            deepEqual(data[2], tokenizer.nextToken(data[1], 1, 1), data[0]);
        });
    });
    
    
    test("symbols", function() {        
        var starts = ['!', '@', '#', '$', '%', '^', '&', '*', '_', '-', '+', '=', '<', '>', '?', '/', 'a', 'z', 'A', 'Z'],
            // this is not an exhaustive list ... should it be?
            nonos = [',', "'", '|', '\\', '.'];
        
        starts.map(function(c) {
            var tok = tokenizer.nextToken(c, 1, 1);
            deepEqual(
                ["",  tokenizer.Token('symbol', c, 1, 1)], 
                [tok.rest, tok.token],
                "symbols may start with " + c
            );
        });
        
        nonos.map(function(c) {
            var err = tokenizer.nextToken(c, 1, 1);
            deepEqual(
                {column: 1, line: 1, message: "no tokens matched", rest: c, status: "error"},
                err,
                "symbols may not start with with " + c
            );
        });
        
        deepEqual(
            {'rest': '', column: 24, line: 1, status: 'success', 'token': tokenizer.Token('symbol', 'j3451kl!@#$%^&*_-+=<>?/', 1, 1)},
            tokenizer.nextToken('j3451kl!@#$%^&*_-+=<>?/', 1, 1),
            "symbols may start with a letter or !@#$%^&*_-+=<>?/, followed by any number of letters, digits or !@#$%^&*()_-+=<>?/"
        );
    });
    
    
    test("error reporting", function() {
        deepEqual(
            tokenizer.tokenize('"abc" \n "qrs\n;123'),
            {status: 'error', 
             error: {message: 'end-of-string not found', line: 2, column: 2,
                     rest: '"qrs\n;123', status: 'error'},
             tokens: [tok('string', 'abc', 1, 1), tok('whitespace', ' \n ', 1, 6)]},
            "string error reporting"
        );
        deepEqual(
            tokenizer.tokenize("abc def,\n,("),
            {status: 'error',
             error: {message: "no tokens matched", line: 1, column: 8,
                     rest: ',\n,(', status: 'error'},
             tokens: [tok('symbol', 'abc', 1, 1), tok('whitespace', ' ', 1, 4),
                      tok('symbol', 'def', 1, 5)]},
            "no token found reporting"
        );
    });


    test("numbers", function () {
        function flt(x, l, c) {
            return tokenizer.Token('float', x, l, c);
        }
        var tks = tokenizer.tokenize("345 03. 3.456 .001 0.00 4..");
        deepEqual(
            [[tokenizer.Token('integer', '345', 1, 1), flt('03.', 1, 5), flt('3.456', 1, 9), 
              flt('.001', 1, 15), flt('0.00', 1, 20), flt('4.', 1, 25)],
             {column: 27, line: 1, message: 'no tokens matched', rest: '.', status: 'error'}],
            [tokenizer.stripTokens(tks.tokens), tks.error],
            "number examples"
        );
    }); 
    

    test("tokenize", function() {
        var tok = tokenizer.Token,
            op = tok('open-paren', "("),
            cp = tok('close-paren', ')'),
            o = tok('integer', '1'),
            p = tok('symbol', '+'),
            s = tok('whitespace', ' ');
        
        deepEqual(
            tokenizer.tokenize("abc 123 4.5 \"hi\" ;bye\n [](){},(,)"),
            {status: 'success', 
             tokens: [tok('symbol',       'abc', 1, 1),  tok('whitespace',    ' ',   1, 4),
                      tok('integer',      '123', 1, 5),  tok('whitespace',    ' ',   1, 8),
                      tok('float',        '4.5', 1, 9),  tok('whitespace',    ' ',   1, 12),
                      tok('string',       'hi',  1, 13), tok('whitespace',    ' ',   1, 17),
                      tok('comment',      'bye', 1, 18), tok('whitespace',    '\n ', 1, 22),
                      tok('open-square',  '[',   2, 2),  tok('close-square',  ']',   2, 3),
                      tok('open-paren',   '(',   2, 4),  tok('close-paren',   ')',   2, 5),
                      tok('open-curly',   '{',   2, 6),  tok('close-curly',   '}',   2, 7),
                      tok('open-special', ',(',  2, 8),  tok('close-special', ',)',  2, 10)]},
            "all of the tokens"
        );
    });
    

    test("strip comments and whitespace", function() {
        var t1 = [
            tokenizer.Token('comment', 'abc'), 
            tokenizer.Token('string', 'hello'), 
            tokenizer.Token('whitespace', '\t\n     \n'), 
            tokenizer.Token('open-paren', '('),
            tokenizer.Token('whitespace', '   \t\t\t\n\t')
        ]; 

        var t2 = tokenizer.stripTokens(t1);

        deepEqual([t1[1], t1[3]], t2, 'all whitespace and comment tokens are discarded by stripping');
    });
}
