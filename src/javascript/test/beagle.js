define(["app/beagle", "app/data", "app/tokens", "app/ast", "libs/maybeerror"], function(beagle, data, tokens, ast, me) {

    module("beagle");
    
    var list = data.List,
        num = data.Number,
        str = data.makeCharList,
        t = tokens.Token,
        exec = beagle.exec;
        
    test("tokenization error", function() {
        var r = exec('123 ",doit now');
        deepEqual(me.error({cause: 'tokenization',
                            error: {line: 1, column: 5,
                                    message: 'end-of-string not found',
                                    rest: '",doit now'}}), 
                  r);
    });
    
    test("parse error", function() {
        var r = exec('123\n [3');
        deepEqual('error', r.status);
        deepEqual('ast parsing', r.value.cause);
    });
    
    test("execution error", function() {
        var r = exec('1 x');
        deepEqual('error', r.status);
        deepEqual('evaluation', r.value.cause);
    });
    
    function lc(l, c) {
        return {line: l, column: c};
    }
    
    test("successful execution", function() {
        var r = exec('(+ 3 2); qrs \n "ab"');
        deepEqual(me.pure({
            tokens: [t('open-paren',  '(',     lc(1,1)), 
                     t('symbol',      '+',     lc(1,2)),
                     t('whitespace',  ' ',     lc(1,3)),
                     t('integer',     '3',     lc(1,4)),
                     t('whitespace',  ' ',     lc(1,5)),
                     t('integer',     '2',     lc(1,6)),
                     t('close-paren', ')',     lc(1,7)),
                     t('comment',     ' qrs ', lc(1,8)),
                     t('whitespace',  '\n ',   lc(1,14)),
                     t('string',      'ab',    lc(2,2))   ],
            results: [num(5), data.makeCharList('ab')],
            asts: [ast.application(ast.symbol('+', {line: 1, column: 2}), 
                                   [ast.number(3, {line: 1, column: 4}), ast.number(2, {line: 1, column: 6})],
                                   {line: 1, column: 1}),
                   ast.list([ast.char('a', {line: 2, column: 3}), 
                             ast.char('b', {line: 2, column: 4})],
                            {line: 2, column: 2})]}), 
            r);
    });

});
