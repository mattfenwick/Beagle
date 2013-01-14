function testASTBuilder(ast, astbuilder, pt, me) {

    module("AST builder");
    
    var pure = me.pure,
        build = astbuilder.build;
        
    test("number", function() {
        deepEqual(
            pure(ast.number(4, 'hi there')),
            build(pt.number(4, 'hi there')));
    });
    
    test("strings", function() {
        deepEqual(
            pure(ast.list('yes'.split('').map(ast.char), ['my meta!'])),
            build(pt.string('yes', ['my meta!'])), 
            'strings become AST lists of chars'
        );
        
        deepEqual(
            pure(ast.list([], 'spaghetti')),
            build(pt.string('', 'spaghetti')));
    });

    test("applications", function () {
        deepEqual(
            pure(ast.application(ast.symbol('+'), [ast.number(2), ast.number(3)], 'for metadata')),
            build(pt.app(pt.symbol('+'), [pt.number(2), pt.number(3)], 'for metadata'))
        );
        
        ok(0, 'need to test error propagation');
    });

    test("objects", function() {
        
    });

    test("getSpecial", function() {
        expExc(function() {
            ast.getNextForm([osp, tok('symbol', 'nope'), csp]);
        }, 'ParseError', 'only cond/lambda/define/set! are valid special forms', 'ValueError');
    });

    test("lists", function() {
    });

    test("define", function() {
    });

    test("set!", function() {
    });

    test("cond", function() {
        expExc(function() {
            ast.getSpecial([osp, cond, os, os, tokSym, tokInt, cs, cs, csp]);
        }, 'ParseError', 'needs two arguments', 'NumArgsError');
        
        expExc(function() {
            ast.getSpecial([osp, cond, tokSym, tokInt, csp]);
        }, 'ParseError', '1st arg must be a list', 'TypeError');
        
        expExc(function() {
            ast.getSpecial([osp, cond, os, tokSym, cs, tokInt, csp]);
        }, 'ParseError', 'and all of its elements must be lists', 'TypeError');
        
        expExc(function() {
            ast.getSpecial([osp, cond, os, os, tokSym, cs, cs, tokInt, csp]);
        }, 'ParseError', 'each with two elements', 'ValueError');
    });

    test("lambda", function() {
        // incorrect usages:
        //   {lambda x 3}              -- 1st arg must be list ...
        //   {lambda [3] "f"}          -- ... of symbols
        //   {lambda [x x] x}          -- no repeated symbols
        expExc(function() {
            ast.getSpecial([osp, lam, os, tokSym, tokSym, cs, tokInt, csp]);
        }, 'ParseError', 'and no repeated symbols', 'ValueError');
        
    });
    
    test("build", function() {
    });

}