define(["app/beagle", "app/data", "app/tokens", "app/ast"], function(beagle, data, tokens, ast) {

    module("beagle");
    
    var list = data.List,
        num = data.Number,
        str = data.makeCharList,
        t = tokens.Token,
        exec = beagle.exec;
        
    test("tokenization error", function() {
        var r = exec('123 ",doit now');
        deepEqual('token error', r.status);
        deepEqual({column: 5, line: 1, rest: '",doit now', message: 'end-of-string not found'}, r.tokenization.error);
    });
    
    test("parse error", function() {
        var r = exec('123\n [3');
        deepEqual([t('integer', '123', {line: 1, column: 1}), t('whitespace', '\n ', {line: 1, column: 4}), 
                   t('open-square', '[', {line: 2, column: 2}), t('integer', '3', {line: 2, column: 3})], r.tokenization);
        deepEqual('parse error', r.status);
        deepEqual({meta: {line: 2, column: 2}, rule: 'list'}, r.asts);
    });
    
    test("execution error", function() {
        var r = exec('1 x');
        deepEqual([t('integer', '1', {line: 1, column: 1}), t('whitespace', ' ', {line: 1, column: 2}),
                   t('symbol', 'x', {line: 1, column: 3})], r.tokenization);
        deepEqual('execution error', r.status);
        deepEqual([ast.number(1, {line: 1, column: 1}), ast.symbol('x', {line: 1, column: 3})], r.asts.result);
    });
    
    test("successful execution", function() {
        var r = exec('3 (+ 3 2) ; qrs \n "ab"');
        deepEqual('success', r.status);
        deepEqual([ast.number(3, {line: 1, column: 1}), 
                   ast.application(ast.symbol('+', {line: 1, column: 4}), 
                                   [ast.number(3, {line: 1, column: 6}), ast.number(2, {line: 1, column: 8})],
                                   {line: 1, column: 3}), 
                   ast.list([ast.char('a', {line: 2, column: 3}), 
                             ast.char('b', {line: 2, column: 4})],
                             {line: 2, column: 2})], r.asts.result);
    });

});
