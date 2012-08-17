function testParser(parse, tokens, testHelper) {

    module("AST construction");

    var tok = tokens.Token,
        app = parse.Application,
        list = parse.ASTList,
        ch  = parse.ASTChar,
        sym = parse.Symbol,
        num = parse.ASTNumber,
        spec = parse.Special,
        str = parse.expandString,
        expExc = testHelper.expectException,
        op = tok('open-paren', '('),
        os = tok('open-square', '['),
        cp = tok('close-paren', ')'),
        cs = tok('close-square', ']'),
        oc = tok('open-curly', '['),
        cc = tok('close-curly', ']'),
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
            parse.getNextForm([str1]), 
            'string tokens become AST lists of chars'
        );
        
        deepEqual({rest: [], result: list([])}, parse.getNextForm([emp]), "empty string token => empty AST list");
    });
    

    test("applications", function () {
        expect(6);

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
            {'rest': [], 'result': app(sym('+'), [str('str1'), num(345)])},
            parse.getNextForm(list1), 
            "an 'Application' is delimited by parentheses ..."
        );

        deepEqual(
            {'rest': [], result: app(sym('+'), 
                                     [app(sym('+'), [str("str1")]), sym('+')] ) },
            parse.getNextForm(list2),
            '... and may be nested'
        );

        expExc(function() {
            parse.getApplication([op, tokInt, cp, tokInt]);
        }, 'ParseError', 'the operator must be an application, cond, lambda, or symbol');

        var d = parse.Define([sym('x'), num('3')]);
        expExc(function() {
            app(sym('y'), d); 
        }, 'ParseError', 'the arguments must be expressions (i.e. not defines or set!s)');
        
        expExc(function() {
            parse.getNextForm([op, cp]);
        }, 'ParseError', 'Application needs a function/symbol -- cannot be empty');

        expExc(function() {
            app(false, []);
        }, 'ParseError', 'trying to create an empty Application throws an exception');
    });


    test("getSpecial", function() {
        var lam = tok('symbol', 'lambda'),
            num1 = tok('integer', '34');

        deepEqual(
            {'rest': [lam], 'result':  spec(sym('lambda'), [num('34')])},
            parse.getNextForm([oc, lam, num1, cc, lam]),
            'special forms are delimited by matching curly braces -- {}'
        );

        expExc(function() {
            parse.getNextForm([oc, lam, oc, lam, cc]);
        }, 'ParseError', 'braces must match');
        
        expExc(function() {
            parse.getNextForm([oc, cc]);
        }, 'ParseError', 'Special needs a symbol -- cannot be empty');

        expExc(function() {
            spec(false, []);
        }, 'ParseError', 'trying to create an empty Special throws an exception');

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
                ["symbols ARE atoms ...",     [barf, op, barf],             {'result': sym("barf"), 'rest': [tok('open-paren', '('), barf]}],
                ["... as are strings",        [tok('string', 'me!')],       {'result': str('me!'),  'rest': []}],
                ["... and integers",          [tok('integer', '145')],      {'result': num(145),    'rest': []}],
                ["... and floats",            [tok('float', '7.23')],       {'result': num(7.23),   'rest': []}],
                ["no atoms in empty list of tokens", [],                    false]
            ];
        
        testCases.map(function(data) {
            deepEqual(data[2], parse.getAtom(data[1]), data[0]);
        });
    });



    test("getList", function() {
        expect(15);

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


        var t = parse.getList([]);
        equal(false, t, "trying to get a list from an empty token stream returns false ... ");

        expExc(function() {
            parse.getList([os]);
        }, 'ParseError', "... while a '[' without a matching ']' throws an error,");

        expExc(function() {
            parse.getList([cs]);
        }, 'ParseError', "and a leading ']' also throws an error");

        var p = parse.getList([os, cs]);
        deepEqual({
            result: list([]),
            rest: []
        }, p, "[] is parsed as an empty list");

        var s = parse.getList([abc]);
        equal(false, s, "you can't get a list from a symbol ...");
      
        equal(false, parse.getList([str, sym]), " ... or from a string");

        var q = parse.getList([os, abc, cs, abc]);
        deepEqual({
            'result': list([sym('abc')]), 
            'rest':  [abc]
        }, q, "a list extends from the opening '[' to the next ']', unless ...");

        var r = parse.getList(t6);
        deepEqual({
            'result': list([
                sym('abc'),
                list([sym('abc')]),
                sym('abc')
            ]),
            'rest': t6.slice(7)
        }, r, "... it has a nested list, in which case it extends to its 'balancing' ']'");

        expExc(function() {
            parse.getList(t7);
        }, 'ParseError', "therefore: a missing 'balancing' ']' throws an error");
      
        var v = parse.getList(t8);
        deepEqual({
            'result': list([
                list([
                    list([
                        list([])])])]),
            'rest': t8.slice(8)
        }, v, "lists may be arbitrarily deeply nested ...");
      
        expExc(function() {
            parse.getList(t9);
        }, 'ParseError', "... as long as the parentheses match");
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
            {'rest': [], 'token': parse.Define([sym('+'), num('3')])},
            parse.getSpecial([oc, def, tokSym, tokInt, cc]),
            'define ...'
        );

        expExc(function() {
            parse.getSpecial([oc, def, tokSym, cc]);
        }, 'ParseError', 'it takes two arguments');

        expExc(function() {
            parse.getSpecial([oc, def, tokSym, tokStr, tokInt, cc]);
        }, 'ParseError', '... no more, no less');

        expExc(function() {
            parse.getSpecial([oc, def, tokInt, tokStr, cc]); // did it throw for the *right* reason?
        }, 'ParseError', 'the first argument must be a Beagle symbol');

        expExc(function() {
            parse.getSpecial([oc, def, tokSym, oc, def, tokSym, tokInt, cc, cc]); 
        }, 'ParseError', 'the 2nd arg must be an expression -- not a statement');

    });


    test("set!", function() {
        // exact same spec as 'define', except:
        //   symbol *must already* be bound in any lexically enclosing environment  
        var set = tok('symbol', 'set!');

        expExc(function() {
            parse.getSpecial([oc, set, tokSym, cc]);
        }, 'ParseError', 'it takes two arguments');

        expExc(function() {
            parse.getSpecial([oc, set, tokSym, tokStr, tokInt, cc]);
        }, 'ParseError', '... no more, no less');

        expExc(function() {
            parse.getSpecial([oc, set, tokInt, tokStr, cc]); // did it throw for the *right* reason?
        }, 'ParseError', 'the first argument must be a Beagle symbol');

        expExc(function() {
            parse.getSpecial([oc, set, tokSym, oc, set, tokSym, tokInt, cc, cc]); 
        }, 'ParseError', 'the 2nd arg must be an expression -- not a statement');
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
        //   {cond [[a b] [c d]]}    -- no 'else' value
        //   {cond x y}              -- 1st arg must be list
        //   {cond [x [y z]] a}      -- all elements of 1st arg must be lists ...
        //   {cond [[x] [y z]] a}    -- with 2 elements
        //   {cond [[{define x 3} 4]] y}  -- can only have expressions ...
        //   {cond [] {set! z 14}}   -- ... anywhere in cond

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
        //   {lambda x 3}        -- 1st arg must be list ...
        //   {lambda [3] "f"}    -- ... of symbols
        //   {lambda [x x] x}    -- no repeated symbols
        //   {lambda [] {define x 3})  -- last arg must be an expression
        //   {lambda [] (+ 3 2) 4}     -- 2nd to (last - 1)th arg must be statements
        //   {lambda [x] (+ x 2)       -- need balanced }

    });
    
    
    test("makeAST", function() {
        var myTokens = [op, tok('symbol', '+'), os, tok('integer', '14'), tok('float', '23.2'), cs, tok('string', 'yes'), cp],
            myRes = [app(sym('+'), [
                               list([num(14), num(23.2)]),
                               str('yes')])];
        
        deepEqual(myRes,
            parse.makeAST(myTokens),
            'to sum it up:  the AST contains nodes of applications, lists, numbers, symbols, and chars'
        );
    });

}