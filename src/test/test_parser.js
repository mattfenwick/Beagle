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
            parse.getNextForm(list1), 
            "an 'Application' is delimited by parentheses ..."
        );

        deepEqual(
            {'rest': [], result: app([sym('+'), 
                                     app([sym('+'), str("str1")]), sym('+')] ) },
            parse.getNextForm(list2),
            '... and may be nested'
        );

        expExc(function() {
            parse.getApplication([op, tokInt, cp, tokInt]);
        }, 'ParseError', 'the operator must be an application, cond, lambda, or symbol', 'TypeError');

        var d = parse.Define([sym('x'), num('3')]);
        expExc(function() {
            app([sym('y'), d]); 
        }, 'ParseError', 'the arguments must be expressions (i.e. not defines or set!s)', 'TypeError');
        
        expExc(function() {
            parse.getNextForm([op, cp]);
        }, 'ParseError', 'Application needs a function/symbol/lambda/cond -- cannot be empty', 'ValueError');

        expExc(function() {
            app(false, []);
        }, 'ParseError', 'trying to create an empty Application throws an exception', 'ValueError');
    });


    test("getSpecial", function() {
        var lam = tok('symbol', 'lambda');

        expExc(function() {
            parse.getNextForm([oc, lam, oc]);
        }, 'ParseError', 'special form braces must match', 'DelimiterError');
        
        expExc(function() {
            parse.getNextForm([oc, cc]);
        }, 'ParseError', 'special forms need a symbol -- cannot be empty', 'ValueError');
        
        expExc(function() {
            parse.getNextForm([oc, tok('symbol', 'nope'), cc]);
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
            deepEqual(data[2], parse.getAtom(data[1]), data[0]);
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


        var t = parse.getList([]);
        equal(false, t, "trying to get a list from an empty token stream returns false ... ");

        expExc(function() {
            parse.getList([os]);
        }, 'ParseError', "... while a '[' without a matching ']' throws an error,", "DelimiterError");

        expExc(function() {
            parse.getList([cs]);
        }, 'ParseError', "and a leading ']' also throws an error", "DelimiterError");

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
        }, 'ParseError', "therefore: a missing 'balancing' ']' throws an error", "DelimiterError");
      
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
        }, 'ParseError', "... as long as the parentheses match", "DelimiterError");
        
        expExc(function() {
            parse.getList([os, oc, tok('symbol', 'define'), tok('symbol', 'c'), tok('integer', '32'), cc, cs]);
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
            {'rest': [], 'result': parse.Define([sym('+'), num('345')])},
            parse.getSpecial([oc, def, tokSym, tokInt, cc]),
            'define ...'
        );

        expExc(function() {
            parse.getSpecial([oc, def, tokSym, cc]);
        }, 'ParseError', 'it takes two arguments', 'NumArgsError');

        expExc(function() {
            parse.getSpecial([oc, def, tokSym, tokStr, tokInt, cc]);
        }, 'ParseError', '... no more, no less', 'NumArgsError');

        expExc(function() {
            parse.getSpecial([oc, def, tokInt, tokStr, cc]); // did it throw for the *right* reason?
        }, 'ParseError', 'the first argument must be a Beagle symbol', 'TypeError');

        expExc(function() {
            parse.getSpecial([oc, def, tokSym, oc, def, tokSym, tokInt, cc, cc]); 
        }, 'ParseError', 'the 2nd arg must be an expression -- not a statement', 'TypeError');

    });


    test("set!", function() {
        // exact same spec as 'define', except:
        //   symbol *must already* be bound in any lexically enclosing environment  
        var set = tok('symbol', 'set!');
        
        deepEqual(
            {'rest': [], 'result': parse.SetBang([sym('+'), num('345')])},
            parse.getSpecial([oc, set, tokSym, tokInt, cc]),
            'set! ...'
        );

        expExc(function() {
            parse.getSpecial([oc, set, tokSym, cc]);
        }, 'ParseError', 'it takes two arguments', 'NumArgsError');

        expExc(function() {
            parse.getSpecial([oc, set, tokSym, tokStr, tokInt, cc]);
        }, 'ParseError', '... no more, no less', 'NumArgsError');

        expExc(function() {
            parse.getSpecial([oc, set, tokInt, tokStr, cc]); // did it throw for the *right* reason?
        }, 'ParseError', 'the first argument must be a Beagle symbol', 'TypeError');

        expExc(function() {
            parse.getSpecial([oc, set, tokSym, oc, set, tokSym, tokInt, cc, cc]); 
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
            {'rest': [tokStr], 'result': parse.Cond([list([list([sy, s]), list([sy, i])]), s])},
            parse.getSpecial([oc, cond, os, os, tokSym, tokStr, cs, os, tokSym, tokInt, cs, cs, tokStr, cc, tokStr]),
            'cond ...'
        );
        
        expExc(function() {
            parse.getSpecial([oc, cond, os, os, tokSym, tokInt, cs, cs, cc]);
        }, 'ParseError', 'needs two arguments', 'NumArgsError');
        
        expExc(function() {
            parse.getSpecial([oc, cond, tokSym, tokInt, cc]);
        }, 'ParseError', '1st arg must be a list', 'TypeError');
        
        expExc(function() {
            parse.getSpecial([oc, cond, os, tokSym, cs, tokInt, cc]);
        }, 'ParseError', 'and all of its elements must be lists', 'TypeError');
        
        expExc(function() {
            parse.getSpecial([oc, cond, os, os, tokSym, cs, cs, tokInt, cc]);
        }, 'ParseError', 'each with two elements', 'ValueError');
        
        expExc(function() {
            parse.getSpecial([oc, cond, os, os, oc, tok('symbol', 'define'), tokSym, tokInt, cc, tokInt, cs, cs, tokInt, cc]);
        }, 'ParseError', 'and both of those elements must be expressions', 'TypeError');
        
        expExc(function() {
            parse.getSpecial([oc, cond, os, cs, oc, tok('symbol', 'define'), tokSym, tokInt, cc, cc]);
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
            {'rest': [tokStr], 'result': parse.Lambda([list([sym('x')]), 
                                                       parse.Define([sym('y'), num('345')]), 
                                                       parse.Application([sy, sym('x'), sym('y')])])},
            parse.getSpecial([oc, lam, os, x, cs, oc, tok('symbol', 'define'), y, tokInt, cc, op, tokSym, x, y, cp, cc, tokStr]),
            'lambda ...'
        );
        
        expExc(function() {
            parse.getSpecial([oc, lam, os, cs, cc]);
        }, 'ParseError', 'needs at least two arguments', 'NumArgsError');
        
        expExc(function() {
            parse.getSpecial([oc, lam, tokSym, tokInt, cc]);
        }, 'ParseError', '1st arg must be a list', 'TypeError');
        
        expExc(function() {
            parse.getSpecial([oc, lam, os, tokInt, cs, tokInt, cc]);
        }, 'ParseError', '... of symbols', 'TypeError');
        
        expExc(function() {
            parse.getSpecial([oc, lam, os, tokSym, tokSym, cs, tokInt, cc]);
        }, 'ParseError', 'and no repeated symbols', 'ValueError');
        
        expExc(function() {
            parse.getSpecial([oc, lam, os, cs, oc, tok('symbol', 'define'), tokSym, tokInt, cc, cc]);
        }, 'ParseError', 'the last body form must be an expression', 'TypeError');

    });
    
    
    test("makeAST", function() {
        var myTokens = [op, tok('symbol', '+'), os, tok('integer', '14'), tok('float', '23.2'), cs, tok('string', 'yes'), cp],
            myRes = [app([sym('+'), 
                          list([num('14'), num('23.2')]),
                          str('yes')])];
        
        deepEqual(myRes,
            parse.makeAST(myTokens),
            'to sum it up:  the AST contains nodes of applications, lists, numbers, symbols, and chars'
        );
    });

}