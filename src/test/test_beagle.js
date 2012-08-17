
function testBeagle(beagle, data, tokens, parser) {

    module("beagle");
    
    var list = data.List,
        num = data.Number,
        str = data.makeCharList;
    
    test("single expressions", function() {
        expect(7);

        var e0 = "456",
            e1 = " ( neg 11 \t \n ) ",
            e2 = "(cons 3 [])",
            e3 = '(car (cdr (cons 1 (cons "blargh" (cons 3 (cons 4 []))))))',
            e4 = ' 3 ; this is a comment',
            e5 = '"abc"123',
            ex = beagle.exec;

        deepEqual([num(456)], ex(e0).result, 'can evaluate an atom');

        deepEqual([num(3)], ex("(+ 1 2)").result, 'can evaluate a simple application');

        deepEqual([num(-11)], ex(e1).result, 'can evaluate a simple application while ignoring whitespace');

        deepEqual([list([num(3)])], ex(e2).result, 'can evaluate a nested application');

        deepEqual([str('blargh')], ex(e3).result, 'can evaluate a deeply nested application');

        deepEqual([num(3)], ex(e4).result, 'can ignore comments');

        var m = true;
        try {
            ex(e5);
            m = false;
        } catch(e) {};
        ok(m, 'strings and symbols must be separated by a space');
    });
    
    
    test("multiple forms", function() {
        expect(2);

        var e1 = "{define x 3} (cons x [])"
            ;

        var r = beagle.exec(e1);
        deepEqual(list([num(3)]), r.result[1], 'multiple expressions can be evaluated at once');
        deepEqual(data.Null(), r.result[0], "and the return value of 'define' is null");
    });


    test("getTokens", function() {
        expect(5);
        var p1 = "(+ 3 3) ; a comment",
            p3 = '(+ (+ (+ 3 3 3)))',
            p5 = "[]",
            p7 = "",
            p8 = "   (+ 3) ;barf me \n [3] ;derr  ",
            tok = tokens.Token,
            op = tok('open-paren', '('),
            cp = tok('close-paren', ')'),
            os = tok('open-square', '['),
            cs = tok('close-square', ']'),
            sym1 = tok('symbol', '+'),
            num1 = tok('integer', '3');

      var a = beagle.getTokens(p1);
      deepEqual(
          [op, sym1, num1, num1, cp],
          a, 'a simple list of three elements, all symbols; whitespace and comments are tossed'
      );

      var c = beagle.getTokens(p3);
      deepEqual([op, sym1, op, sym1, op, sym1, num1, num1, num1, cp, cp, cp], c, 'deeply nested lists are no problem ...');

      var e = beagle.getTokens(p5);
      deepEqual([os, cs], e, 'the empty list is easy to parse');
  
      var g = beagle.getTokens(p7);
      deepEqual(g, [], "the empty string can be tokenized but doesn't return any tokens");

      deepEqual([op, sym1, num1, cp, os, num1, cs], beagle.getTokens(p8), 'multiple sexpressions are fine');
    });

}
