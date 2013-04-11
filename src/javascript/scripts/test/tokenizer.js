define(["app/tokens", "app/tokenizer", "libs/maybeerror", "test/helper"], function(tokens, tokenizer, maybeerror, testHelper) {

    module("tokenizer");    
    
    function loc(l, c) {
        return {
            line: l,
            column: c
        };
    }

    test("error reporting", function() {
        var tok = tokens.Token;
        deepEqual(
            tokenizer.tokenize('"abc" \n "qrs\n;123'),
            maybeerror.error({message: 'end-of-string not found', line: 2, 
                              column: 2, rest: '"qrs\n;123'}),
            "string error reporting"
        );
        deepEqual(
            tokenizer.tokenize("abc def,\n,("),
            maybeerror.error({message: "no tokens matched", line: 1,
                              column: 8, rest: ',\n,('}),
            "no token found reporting"
        );
    });

    test("position reporting", function() {
        deepEqual(
            maybeerror.pure([{type: 'token', tokentype: 'whitespace', value: '  \n \t', meta: loc(1, 1)},
                             {type: 'token', tokentype: 'integer', value: '123', meta: loc(2, 3)}]),
            tokenizer.tokenize('  \n \t123'));
    });
    
    test("tokenize", function() {
        var tok = tokens.Token;
        
        deepEqual(
            tokenizer.tokenize("abc 123 4.5 \"hi\" ;bye\n [](){}"),
            maybeerror.pure([tok('symbol', 'abc', loc(1, 1)),  tok('whitespace',    ' ',   loc(1, 4)),
                      tok('integer',       '123', loc(1, 5)),  tok('whitespace',    ' ',   loc(1, 8)),
                      tok('float',         '4.5', loc(1, 9)),  tok('whitespace',    ' ',   loc(1, 12)),
                      tok('string',        'hi',  loc(1, 13)), tok('whitespace',    ' ',   loc(1, 17)),
                      tok('comment',       'bye', loc(1, 18)), tok('whitespace',    '\n ', loc(1, 22)),
                      tok('open-square',   '[',   loc(2, 2)),  tok('close-square',  ']',   loc(2, 3)),
                      tok('open-paren',    '(',   loc(2, 4)),  tok('close-paren',   ')',   loc(2, 5)),
                      tok('open-curly',    '{',   loc(2, 6)),  tok('close-curly',   '}',   loc(2, 7))]),
            "all of the tokens"
        );
    });

});
