function testParser(reify, data, tokens, testHelper) {

    module("AST construction");

    var tok = tokens.Token,
        app = data.Application,
        list = data.List,
        ch = data.Char,
        sym = data.Symbol,
        num = data.Number,
        str = data.makeCharList,
        expExc = testHelper.expectException,
        op = tok('open-paren', '('),
        os = tok('open-square', '['),
        cp = tok('close-paren', ')'),
        cs = tok('close-square', ']');

    
    test("strings", function() {
        var str1 = tok('string', "yes"),
            emp = tok('string', "");
    
        deepEqual(
            {rest: [], result: str('yes')},
            reify.getNextForm([str1]), 
            'string tokens are reified into lists of chars'
        );
        
        deepEqual({rest: [], result: list([])}, reify.getNextForm([emp]), "empty string token => empty Lisp list");
    });
    

    test("applications", function () {
        expect(4);

        var list1 = [
                op,
                tok('symbol', "+"),
                tok('string', 'str1'),
                tok('integer', "345"),
                cp
            ],
            list2 = [
                op,
                tok('symbol', "+"),
                op,
                tok('symbol', "-"),
                tok('string', ""),
                cp,
                tok('symbol', '>>>'),
                cp
            ];


        deepEqual(
            {'rest': [], 'result': app(sym('+'), [str('str1'), num(345)])},
            reify.getNextForm(list1), 
            "an 'Application' is delimited by parentheses ..."
        );

        deepEqual(
            {'rest': [], result: app(sym('+'), 
                                     [app(sym('-'), 
                                          [str("")]),
                                      sym('>>>')])},
            reify.getNextForm(list2),
            '... and may be nested'
        );
        
        expExc(function() {
            reify.getNextForm([op, cp]);
        }, 'ValueError', 'Application needs (at least) a function or special form -- an empty one cannot be reified');
    });

    
    test("getAtom", function() {
        var barf = tok('symbol', 'barf'),
            testCases = [
                ["'(' is not an atom",        [op, barf],                   false],
                [" ... nor is ')' ...",       [close, barf, op],            false],
                [" ... nor is a comment ...", [tok('comment', 'blargh')],   false],
                [" ... nor is whitespace",    [tok('whitespace', '\n \t')], false],
                ["symbols ARE atoms ...",     [barf, op, barf],             {'result': sym("barf"), 'rest': [tok('open-paren', '('), barf]}],
                ["... as are strings",        [tok('string', 'me!')],       {'result': str('me!'),  'rest': []}],
                ["no atoms in empty list of tokens", [],                    false]
            ];
        
        testCases.map(function(data) {
            deepEqual(data[2], reify.getAtom(data[1]), data[0]);
        });
    });



    test("getList", function() {
        expect(15);

        var abc = tok('symbol', 'abc'),
            bleh = tok('string', 'bleh'),
            t6 = [os, abc, os, abc, cs, abc, cs, os, abc],
            t7 = [os, abc, os, abc, cs, abc],
            t8 = [os, os, os, os, cs, cs, cs, cs, abc],
            t9 = [os, os, os, os, cs, cs, cs],
            ll = Data.ListLiteral;


        var t = reify.getList([]);
        equal(false, t, "trying to get a list from an empty token stream returns false ... ");

        expExc(function() {
            reify.getList([os]);
        }, 'ParseError', "... while a '[' without a matching ']' throws an error,");

        expExc(function() {
            reify.getList([cs]);
        }, 'ParseError', "and a leading ']' also throws an error");

        var p = reify.getList([os, cs]);
        deepEqual({
            result: ll([]),
            rest: []
        }, p, "[] is parsed as an empty list");

        var s = reify.getList([abc]);
        equal(false, s, "you can't get a list from a symbol ...");
      
        equal(false, reify.getList([str, sym]), " ... or from a string");

        var q = reify.getList([os, abc, cs, abc]);
        deepEqual({
            'result': ll([sym('abc')]), 
            'rest':  [abc]
        }, q, "a list extends from the opening '[' to the next ']', unless ...");

        var r = reify.getList(t6);
        deepEqual({
            'result': ll([
                sym('abc'),
                ll([sym('abc')]),
                sym('abc')
            ]),
            'rest': t6.slice(7)
        }, r, "... it has a nested list, in which case it extends to its 'balancing' ']'");

        expExc(function() {
            reify.getList(t7);
        }, 'ParseError', "therefore: a missing 'balancing' ']' throws an error");
      
        var v = reify.getList(t8);
        deepEqual({
            'result': ll([
                ll([
                    ll([
                        ll([])])])]),
            'rest': t8.slice(8)
        }, v, "lists may be arbitrarily deeply nested ...");
      
        expExc(function() {
            reify.getList(t9);
        }, 'ParseError', "... as long as the parentheses match");
    });

}