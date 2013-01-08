function testAST(ast, tokens, testHelper) {

    module("AST construction");

    var tok = tokens.Token,
        app = ast.Application,
        list = ast.ASTList,
        ch  = ast.ASTChar,
        sym = ast.Symbol,
        num = ast.ASTNumber,
        spec = ast.Special,
        str = ast.expandString,
        expExc = testHelper.expectException,
        op  =  tok('open-paren'    ,  '('),
        os  =  tok('open-square'   ,  '['),
        cp  =  tok('close-paren'   ,  ')'),
        cs  =  tok('close-square'  ,  ']'),
        oc  =  tok('open-curly'    ,  '{'),
        cc  =  tok('close-curly'   ,  '}'),
        osp =  tok('open-special'  ,  '~('),
        csp =  tok('close-special' ,  '~)'),
        tokSym = tok('symbol', "+"),
        tokStr = tok('string', 'str1'),
        tokInt = tok('integer', '345');

    
    test("strings", function() {
        // examples:
        //   ""         => the empty string
        //   "abc'def"  => a string of 7 characters
        //
        // explanation:
        //   strings become lists of chars in the AST
        //
        // incorrect usages:
        //   "abc"def"  -- can't put double-quotes in strings
        //   "xyz       -- must have ending double-quote

        var str1 = tok('string', "yes"),
            emp = tok('string', "");
    
        deepEqual(
            {rest: [], result: str('yes')},
            ast.getNextForm([str1]), 
            'string tokens become AST lists of chars'
        );
        
        deepEqual({rest: [], result: list([])}, ast.getNextForm([emp]), "empty string token => empty AST list");
    });
    
    
    test("tables", function() {
        // examples:
        //   { } 
        //   { "abc" 123 "def" [1 18 29] "qrs" (+ 3 2)  }
        var s1 = tok('string', "abc"),
            n1 = tok('integer', '123'),
            r1 = app([sym('object'), list([])]),
            r2 = app([sym('object'), list([list([str("abc"), num('123')])])]);
        deepEqual({rest: [oc], result: r1}, ast.getNextForm([oc, cc, oc]));
        deepEqual({rest: [], result: r2}, ast.getNextForm([oc, s1, n1, cc]));
        
        expExc(function() {
            ast.getNextForm([oc, s1, n1, tok('string', 'def'), cc, s1]);
        }, 'ParseError', 'object literal needs one key for each value');
    });
    

    test("applications", function () {
        // examples:
        //   (+ 3 2)                    => 5
        //   (g)                        => evaluates a function of 0 arguments
        //   ({lambda [x] (+ x x)} 13)  => 26
        //   ({cond [true -] +} 37 9)   => 28
        //   ((f x) y)                  => applies f to x, applies result to y
        //
        // explanation:
        //   '(', symbol/lambda/cond/application, 0 or more expressions, ')'
        //
        // incorrect usages:
        //   (13 15), ([1 2] 4), ("abc") -- need symbol/lambda/cond/application in 1st position
        //   ()                -- can't have empty application
        //   (x y z            -- need balanced parentheses
        //   (f {define x 3})  -- arguments must be expressions, not statements

        var list1 = [ op,    tokSym,  tokStr,   tokInt,  cp ],
            list2 = [ op, tokSym, op, tokSym,  tokStr, cp, tokSym, cp  ];


        deepEqual(
            {'rest': [], 'result': app([sym('+'), str('str1'), num('345')])},
            ast.getNextForm(list1), 
            "an 'Application' is delimited by parentheses ..."
        );

        deepEqual(
            {'rest': [], result: app([sym('+'), 
                                     app([sym('+'), str("str1")]), sym('+')] ) },
            ast.getNextForm(list2),
            '... and may be nested'
        );

        expExc(function() {
            ast.getApplication([op, tokInt, cp, tokInt]);
        }, 'ParseError', 'the operator must be an application, cond, lambda, or symbol', 'TypeError');

        var d = ast.Define([sym('x'), num('3')]);
        expExc(function() {
            app([sym('y'), d]); 
        }, 'ParseError', 'the arguments must be expressions (i.e. not defines or set!s)', 'TypeError');
        
        expExc(function() {
            ast.getNextForm([op, cp]);
        }, 'ParseError', 'Application needs a function/symbol/lambda/cond -- cannot be empty', 'ValueError');

        expExc(function() {
            app(false, []);
        }, 'ParseError', 'trying to create an empty Application throws an exception', 'ValueError');
    });


    test("getSpecial", function() {
        var lam = tok('symbol', 'lambda');

        expExc(function() {
            ast.getNextForm([osp, lam]);
        }, 'ParseError', 'an opening delimiter needs a corresponding closing one', 'DelimiterError');
        
        expExc(function() {
            ast.getNextForm([osp, csp]);
        }, 'ParseError', 'special forms need a symbol -- cannot be empty', 'ValueError');
        
        expExc(function() {
            ast.getNextForm([osp, tok('symbol', 'nope'), csp]);
        }, 'ParseError', 'only cond/lambda/define/set! are valid special forms', 'ValueError');
    });

    
    test("getAtom", function() {
        // examples:
        //   abc    => a symbol
        //   145    => a number
        //   7.23   => also a number
        //   "xyz"  => a string
        //
        // explanation:
        //   a one-token syntactic unit

        var barf = tok('symbol', 'barf'),
            testCases = [
                ["'(' is not an atom",        [op, barf],                   false],
                [" ... nor is ')' ...",       [close, barf, op],            false],
                [" ... nor is a comment ...", [tok('comment', 'blargh')],   false],
                [" ... nor is whitespace",    [tok('whitespace', '\n \t')], false],
                ["symbols ARE atoms ...",     [barf, op, barf],             {'result': sym("barf"),  'rest': [tok('open-paren', '('), barf]}],
                ["... as are strings",        [tok('string', 'me!')],       {'result': str('me!'),   'rest': []}],
                ["... and integers",          [tok('integer', '145')],      {'result': num('145'),   'rest': []}],
                ["... and floats",            [tok('float', '7.23')],       {'result': num('7.23'),  'rest': []}],
                ["no atoms in empty list of tokens", [],                    false]
            ];
        
        testCases.map(function(data) {
            deepEqual(data[2], ast.getAtom(data[1]), data[0]);
        });
    });



    test("getList", function() {
        // examples:
        //   []            => the empty list
        //   [1 a "" (a)]  => a list with a number, symbol, string, and application
        //   [[]]          => a nested list
        //
        // explanation:
        //   '[', 0 or more expressions, ']'
        //
        // incorrect usages:
        //   [1 2 3             -- needs balanced brackets
        //   [{define x 3} 14]  -- can only contain expressions, not statements

        var abc = tok('symbol', 'abc'),
            bleh = tok('string', 'bleh'),
            t6 = [os, abc, os, abc, cs, abc, cs, os, abc],
            t7 = [os, abc, os, abc, cs, abc],
            t8 = [os, os, os, os, cs, cs, cs, cs, abc],
            t9 = [os, os, os, os, cs, cs, cs],
            ll = Data.ListLiteral;


        var t = ast.getList([]);
        equal(false, t, "trying to get a list from an empty token stream returns false ... ");

        expExc(function() {
            ast.getList([os]);
        }, 'ParseError', "... while a '[' without a matching ']' throws an error,", "DelimiterError");

        expExc(function() {
            ast.getList([cs]);
        }, 'ParseError', "and a leading ']' also throws an error", "DelimiterError");

        var p = ast.getList([os, cs]);
        deepEqual({
            result: list([]),
            rest: []
        }, p, "[] is parsed as an empty list");

        var s = ast.getList([abc]);
        equal(false, s, "you can't get a list from a symbol ...");
      
        equal(false, ast.getList([str, sym]), " ... or from a string");

        var q = ast.getList([os, abc, cs, abc]);
        deepEqual({
            'result': list([sym('abc')]), 
            'rest':  [abc]
        }, q, "a list extends from the opening '[' to the next ']', unless ...");

        var r = ast.getList(t6);
        deepEqual({
            'result': list([
                sym('abc'),
                list([sym('abc')]),
                sym('abc')
            ]),
            'rest': t6.slice(7)
        }, r, "... it has a nested list, in which case it extends to its 'balancing' ']'");

        expExc(function() {
            ast.getList(t7);
        }, 'ParseError', "therefore: a missing 'balancing' ']' throws an error", "DelimiterError");
      
        var v = ast.getList(t8);
        deepEqual({
            'result': list([
                list([
                    list([
                        list([])])])]),
            'rest': t8.slice(8)
        }, v, "lists may be arbitrarily deeply nested ...");
      
        expExc(function() {
            ast.getList(t9);
        }, 'ParseError', "... as long as the parentheses match", "DelimiterError");
        
        expExc(function() {
            ast.getList([os, osp, tok('symbol', 'define'), tok('symbol', 'c'), tok('integer', '32'), csp, cs]);
        }, 'ParseError', 'lists may not contain statements', 'TypeError');
    });


    test("define", function() {
        // examples:
        //   {define x 3}
        //   {define abc {lambda [x] (cons x [1 2])}}
        //
        // explanation:
        //   symbol *must not* be bound in current lexical environment
        //   '{', 'define', symbol, expression, '}'   
        //
        // incorrect usages:
        //   {define 3 []}                -- need symbol as 1st arg
        //   {define x {set! y 3})        -- need expression as 2nd arg
        //   {define z}, {define a 1 2}   -- need two args
        //   {define b "four"             -- need closing '}'
        var def = tok('symbol', 'define');

        deepEqual(
            {'rest': [], 'result': ast.Define([sym('+'), num('345')])},
            ast.getSpecial([osp, def, tokSym, tokInt, csp]),
            'define ...'
        );

        expExc(function() {
            ast.getSpecial([osp, def, tokSym, csp]);
        }, 'ParseError', 'it takes two arguments', 'NumArgsError');

        expExc(function() {
            ast.getSpecial([osp, def, tokSym, tokStr, tokInt, csp]);
        }, 'ParseError', '... no more, no less', 'NumArgsError');

        expExc(function() {
            ast.getSpecial([osp, def, tokInt, tokStr, csp]); // did it throw for the *right* reason?
        }, 'ParseError', 'the first argument must be a Beagle symbol', 'TypeError');

        expExc(function() {
            ast.getSpecial([osp, def, tokSym, osp, def, tokSym, tokInt, csp, csp]); 
        }, 'ParseError', 'the 2nd arg must be an expression -- not a statement', 'TypeError');

    });


    test("set!", function() {
        // exact same spec as 'define', except:
        //   symbol *must already* be bound in any lexically enclosing environment  
        var set = tok('symbol', 'set!');
        
        deepEqual(
            {'rest': [], 'result': ast.SetBang([sym('+'), num('345')])},
            ast.getSpecial([osp, set, tokSym, tokInt, csp]),
            'set! ...'
        );

        expExc(function() {
            ast.getSpecial([osp, set, tokSym, csp]);
        }, 'ParseError', 'it takes two arguments', 'NumArgsError');

        expExc(function() {
            ast.getSpecial([osp, set, tokSym, tokStr, tokInt, csp]);
        }, 'ParseError', '... no more, no less', 'NumArgsError');

        expExc(function() {
            ast.getSpecial([osp, set, tokInt, tokStr, csp]); // did it throw for the *right* reason?
        }, 'ParseError', 'the first argument must be a Beagle symbol', 'TypeError');

        expExc(function() {
            ast.getSpecial([osp, set, tokSym, osp, set, tokSym, tokInt, csp, csp]); 
        }, 'ParseError', 'the 2nd arg must be an expression -- not a statement', 'TypeError');
    });


    test("cond", function() {
        // examples:
        //   {cond [[true y]] z}             => evaluates to y
        //   {cond [[false b] [true d]] e}   => evaluates to d
        //   {cond [] f}                     => evaluates to f
        //
        // explanation:
        //   '{', 'cond', list of (two-element lists where both are expressions), expression, '}'
        //
        // incorrect usages:
        //   {cond [[a b] [c d]]}         -- no 'else' value
        //   {cond x y}                   -- 1st arg must be list
        //   {cond [x [y z]] a}           -- all elements of 1st arg must be lists ...
        //   {cond [[x] [y z]] a}         -- with 2 elements
        //   {cond [[{define x 3} 4]] y}  -- can only have expressions ...
        //   {cond [] {set! z 14}}        -- ... anywhere in cond
        var cond = tok('symbol', 'cond'),
            s = str('str1'),
            sy = sym('+'),
            i = num('345');
        
        // {cond [[+ "str1"][+ 345]] "str"} -- don't worry that it's non-sensical
        deepEqual(
            {'rest': [tokStr], 'result': ast.Cond([list([list([sy, s]), list([sy, i])]), s])},
            ast.getSpecial([osp, cond, os, os, tokSym, tokStr, cs, os, tokSym, tokInt, cs, cs, tokStr, csp, tokStr]),
            'cond ...'
        );
        
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
        
        expExc(function() {
            ast.getSpecial([osp, cond, os, os, osp, tok('symbol', 'define'), tokSym, tokInt, csp, tokInt, cs, cs, tokInt, csp]);
        }, 'ParseError', 'and both of those elements must be expressions', 'TypeError');
        
        expExc(function() {
            ast.getSpecial([osp, cond, os, cs, osp, tok('symbol', 'define'), tokSym, tokInt, csp, csp]);
        }, 'ParseError', 'the else-value must also be an expression', 'TypeError');
    });


    test("lambda", function() {
        // examples:
        //   {lambda [] false}      -- function of zero args
        //   {lambda [x] (+ x 3)}   -- function of 1 arg
        //   {lambda [x] {define y x} [x y]}  -- function with internal define/set!'s
        //
        // explanation:
        //   '{', 'lambda', list of symbols, 0 or more statements, expression, '}'
        //
        // incorrect usages:
        //   {lambda x 3}              -- 1st arg must be list ...
        //   {lambda [3] "f"}          -- ... of symbols
        //   {lambda [x x] x}          -- no repeated symbols
        //   {lambda [] {define x 3})  -- last arg must be an expression
        //   {lambda [x] (+ x 2)       -- need balanced }
        //
        // lifted restrictions:
        //   {lambda [] (+ 3 2) 4}     -- 2nd to (last - 1)th arg must be statements
        // 
        var lam = tok('symbol', 'lambda'),
            s = str('str1'),
            sy = sym('+'),
            i = num('345'),
            x = tok('symbol', 'x'),
            y = tok('symbol', 'y');
        
        // {lambda [x] {define y 345} (+ x y)}
        deepEqual(
            {'rest': [tokStr], 'result': ast.Lambda([list([sym('x')]), 
                                                       ast.Define([sym('y'), num('345')]), 
                                                       ast.Application([sy, sym('x'), sym('y')])])},
            ast.getSpecial([osp, lam, os, x, cs, osp, tok('symbol', 'define'), y, tokInt, csp, op, tokSym, x, y, cp, csp, tokStr]),
            'lambda ...'
        );
        
        expExc(function() {
            ast.getSpecial([osp, lam, os, cs, csp]);
        }, 'ParseError', 'needs at least two arguments', 'NumArgsError');
        
        expExc(function() {
            ast.getSpecial([osp, lam, tokSym, tokInt, csp]);
        }, 'ParseError', '1st arg must be a list', 'TypeError');
        
        expExc(function() {
            ast.getSpecial([osp, lam, os, tokInt, cs, tokInt, csp]);
        }, 'ParseError', '... of symbols', 'TypeError');
        
        expExc(function() {
            ast.getSpecial([osp, lam, os, tokSym, tokSym, cs, tokInt, csp]);
        }, 'ParseError', 'and no repeated symbols', 'ValueError');
        
        expExc(function() {
            ast.getSpecial([osp, lam, os, cs, osp, tok('symbol', 'define'), tokSym, tokInt, csp, csp]);
        }, 'ParseError', 'the last body form must be an expression', 'TypeError');

    });
    
    
    test("makeAST", function() {
        var myTokens = [op, tok('symbol', '+'), os, tok('integer', '14'), tok('float', '23.2'), cs, tok('string', 'yes'), cp],
            myRes = [app([sym('+'), 
                          list([num('14'), num('23.2')]),
                          str('yes')])];
        
        deepEqual(myRes,
            ast.makeAST(myTokens),
            'to sum it up:  the AST contains nodes of applications, lists, numbers, symbols, and chars'
        );
    });

}