
function testTokenizer(tokens, tokenizer, maybeerror, testHelper) {

    module("tokenizer");    

    test("error reporting", function() {
        var tok = tokens.Token;
        deepEqual(
            tokenizer.tokenize('"abc" \n "qrs\n;123'),
            maybeerror.error({error: {message: 'end-of-string not found', line: 2, 
                                      column: 2, rest: '"qrs\n;123'},
                              tokens: [tok('string', 'abc', 1, 1), tok('whitespace', ' \n ', 1, 6)]}),
            "string error reporting"
        );
        deepEqual(
            tokenizer.tokenize("abc def,\n,("),
            maybeerror.error({error: {message: "no tokens matched", line: 1,
                                      column: 8, rest: ',\n,('},
                              tokens: [tok('symbol', 'abc', 1, 1), tok('whitespace', ' ', 1, 4),
                                       tok('symbol', 'def', 1, 5)]}),
            "no token found reporting"
        );
    });

    
    test("tokenize", function() {
        var tok = tokens.Token,
            op = tok('open-paren', "("),
            cp = tok('close-paren', ')'),
            o = tok('integer', '1'),
            p = tok('symbol', '+'),
            s = tok('whitespace', ' ');
        
        deepEqual(
            tokenizer.tokenize("abc 123 4.5 \"hi\" ;bye\n [](){},(,)"),
            maybeerror.pure([tok('symbol',       'abc', 1, 1),  tok('whitespace',    ' ',   1, 4),
                      tok('integer',      '123', 1, 5),  tok('whitespace',    ' ',   1, 8),
                      tok('float',        '4.5', 1, 9),  tok('whitespace',    ' ',   1, 12),
                      tok('string',       'hi',  1, 13), tok('whitespace',    ' ',   1, 17),
                      tok('comment',      'bye', 1, 18), tok('whitespace',    '\n ', 1, 22),
                      tok('open-square',  '[',   2, 2),  tok('close-square',  ']',   2, 3),
                      tok('open-paren',   '(',   2, 4),  tok('close-paren',   ')',   2, 5),
                      tok('open-curly',   '{',   2, 6),  tok('close-curly',   '}',   2, 7),
                      tok('open-special', ',(',  2, 8),  tok('close-special', ',)',  2, 10)]),
            "all of the tokens"
        );
    });

}
