
function testTokens(tokens, testHelper) {

    module("tokens");
    var expectExc = testHelper.expectException;
    
    test("Token construction", function() {
        var ts = {
            'open-paren'   : '(',
            'close-paren'  : ')',
            'open-curly'   : '{',
            'close-curly'  : '}',
            'open-square'  : '[',
            'close-square' : ']',
            'open-special' : ',(',
            'close-special': ',)',
            'whitespace'   : ' \t\n',
            'comment'      : ';hi',
            'integer'      : '27',
            'float'        : '31.4',
            'symbol'       : 'abc',
            'string'       : '"oops"'
        }, 
            i = 2; // arbitrary number so things don't get boring
        
        for(var t in ts) {
            deepEqual({type: 'token', tokentype: t,
                       meta: {line: i * 2, column: i * 3},
                       value: ts[t]}, 
                      tokens.Token(t, ts[t], {line: i * 2, column: i * 3}));
            i++; // keep it exciting !!!
        }
    });
    
    test("invalid token construction", function() {    
        expectExc(function() {
            tokens.Token('blargh?', 'hi');
        }, 'ValueError', 'invalid token type causes exception');
    });
    
    test("number regex", function () {
        var nums = ['03.', '3.456', '.001', '0.00'];
        nums.map(function(n) {
            deepEqual(
                [n, n],
                n.match(tokens.REGEXES.float)
            );
        });
        deepEqual(['4.', '4.'], '4..'.match(tokens.REGEXES.float));
        deepEqual(['345', '345'], '345'.match(tokens.REGEXES.integer));
    });
    
    test("symbol regex", function() {        
        var starts = ['!', '@', '#', '$', '%', '^', '&', '*', '_', '-', '+', '=', '<', '>', '?', '/', 'a', 'z', 'A', 'Z'],
            // this is not an exhaustive list ... should it be?
            nonos = [',', "'", '|', '\\', '.'];
        
        starts.map(function(c) {
            var tok = c.match(tokens.REGEXES.symbol);
            deepEqual(
                [c, c], 
                tok,
                "symbols may start with " + c
            );
        });
        
        nonos.map(function(c) {
            var err = c.match(tokens.REGEXES.symbol);
            deepEqual(
                null,
                err,
                "symbols may not start with with " + c
            );
        });
        
        deepEqual(
            ['j3451kl!@#$%^&*_-+=<>?/', 'j3451kl!@#$%^&*_-+=<>?/'],
            'j3451kl!@#$%^&*_-+=<>?/'.match(tokens.REGEXES.symbol),
            "symbols may start with a letter or !@#$%^&*_-+=<>?/, followed by any number of letters, digits or !@#$%^&*()_-+=<>?/"
        );
    });


    test("Priorities", function() {
        deepEqual(14, tokens.PRIORITIES.length, 'must be a priority for each of the 14 token types');
    });
    
    test("token regexes", function() {
        expect(16);
        
        var testCases = [
                ['open-paren',   "((duh",       ['(', '(']       ],
                
                ['close-paren',  ") bleh",      [')', ')']       ],
                
                ['symbol',       "abc)(()",     ['abc', 'abc']   ],
                
                ['integer',      "12345 )",     ['12345', '12345']],
                
                ['float',        ".23 bleh",    ['.23', '.23']    ],
                
                ['whitespace',   '\n\them\n',   ['\n\t', '\n\t'] ],
                
                ["comment",      "; out\nme",   ['; out', ' out'] ],
                                 
                ['string',       '"abc" ',      ['"abc"', 'abc']  ],
                
                ['open-square',  "[bl ; h",     ['[', '[']     ],
                
                ['close-square', "][][",        [']', ']']     ],
                
                ['open-curly',   "{me ; h",     ['{', '{']     ],
                
                ['close-curly',  "} 11[][",     ['}', '}']     ],
                
                ['string',       '"ab cd" f',   ['"ab cd"', 'ab cd']    ],
                                 
                ["symbol",       'wh;at',       ['wh', 'wh']       ],

                ["open-special", ",(abc",       [',(', ',(']       ],

                ["close-special",",))[]",       [',)', ',)']       ]
        ];
        
        testCases.map(function(data) {
            deepEqual(data[2], data[1].match(tokens.REGEXES[data[0]]), data[0]);
        });
    });

}
