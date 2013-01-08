
function testTokens(tokens, testHelper) {

    module("tokens");
    var expectExc = testHelper.expectException;
    
    test("Token", function() {
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
                       line: i * 2, column: i * 3,
                       value: ts[t]}, 
                      tokens.Token(t, ts[t], i * 2, i * 3));
            i++;
        }
        
        expectExc(function() {
            tokens.Token('blargh?', 'hi', 1, 2);
        }, '?', 'invalid token type causes exception');
    });
    
    test("Regexes", function() {
        ok(0, "not tested");
    });

    test("Priorities", function() {
        ok(0, "not tested");
    });
}
