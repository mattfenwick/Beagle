function testAST(ast, testHelper) {

    module("AST data");

    var exp = testHelper.expectException;

    
    test("number", function() {
        deepEqual({type: 'astnode', asttype: 'number', value: 13, meta: 'abc'}, ast.number(13, 'abc'));
    
        exp(function() {
            ast.number('oops');
        }, 'TypeError');
    });
    
    
    test("char", function() {
        deepEqual({type: 'astnode', asttype: 'char', value: 'i'}, ast.char('i'), 'no meta-data on chars');
    
        exp(function() {
            ast.char([1]);
        }, 'TypeError');
        
        exp(function() {
            ast.char('abc');
        }, 'ValueError');
    });
    
    
    test("symbol", function() {
        deepEqual({type: 'astnode', asttype: 'symbol', value: 'abcd', meta: 'hi'}, ast.symbol('abcd', 'hi'));
        
        exp(function() {
            ast.symbol(14);
        }, 'TypeError');
    });
    
    
    test("application", function () {
        deepEqual(
            {type: 'astnode', asttype: 'application', operator: ast.symbol('+'), 'arguments': [ast.number(2)], meta: 'add'},
            ast.application(ast.symbol('+'), [ast.number(2)], 'add')
        );
        
        exp(function() {
            ast.application(ast.symbol('+'), 'oops');
        }, 'TypeError');
    });

    
    test("list", function() {
        deepEqual(
            {type: 'astnode', asttype: 'list', elements: [ast.number(2), ast.symbol('xyz')], meta: 9},
            ast.list([ast.number(2), ast.symbol('xyz')], 9)
        );
        
        exp(function() {
            ast.list('uh-oh');
        }, 'TypeError');
    });


    test("define", function() {
        deepEqual(
            {type: 'astnode', asttype: 'define', symbol: 'q', value: ast.number(14), meta: 'hi there'},
            ast.define('q', ast.number(14), 'hi there')
        );
        
        exp(function() {
            ast.define(14, 'oops');
        }, 'TypeError');
    });


    test("set", function() {
        deepEqual(
            {type: 'astnode', asttype: 'set', symbol: 'q', value: ast.number(14), meta: 'hi there'},
            ast.set('q', ast.number(14), 'hi there')
        );
        
        exp(function() {
            ast.set(14, 'oops');
        }, 'TypeError');
    });


    test("cond", function() {
        deepEqual(
            {type: 'astnode', asttype: 'cond', branches: [[ast.symbol('false'), ast.number(3)]], elseValue: ast.number(2), meta: 3},
            ast.cond([[ast.symbol('false'), ast.number(3)]], ast.number(2), 3)
        );
        
        exp(function() {
            ast.cond('hi there', 'nope');
        }, 'TypeError');
        
        exp(function() {
            ast.cond([[ast.symbol('true')]], 'uh-oh');
        }, 'ValueError');
        
        exp(function() {
            ast.cond(['wow, not good'], 'uh-oh');
        }, 'TypeError');
    });


    test("lambda", function() {
        deepEqual(
            {type: 'astnode', asttype: 'lambda', parameters: ['a', 'b'], bodies: [], returnValue: ast.symbol('a'), meta: 'no'},
            ast.lambda(['a', 'b'], [], ast.symbol('a'), 'no')
        );
        
        exp(function() {
            ast.lambda('abc', [], 3);
        }, 'TypeError');
        
        exp(function() {
            ast.lambda(['a', 'a'], [], 4);
        }, 'ValueError');
        
        exp(function() {
            ast.lambda([], 4, 'uh-oh');
        }, 'TypeError');
    });

}