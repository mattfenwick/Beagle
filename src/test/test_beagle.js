
function testBeagle(beagle, data, parse) {

    module("beagle");
    
    var list = data.List,
        num = data.Number,
        str = data.String;
    
    test("single expressions", function() {
        expect(7);

        var e0 = "456",
            e1 = " ( list 11 \t \n ) ",
            e2 = "(cons 3 (list))",
            e3 = '(car (cdr (cons 1 (cons "blargh" (cons 3 (cons 4 (list)))))))',
            e4 = ' 3 ; this is a comment',
            e5 = '"abc"123',
            ex = beagle.exec;

        deepEqual([num(456)], ex(e0).result, 'can evaluate an atom');

        deepEqual([list([num(1), num(2)])], ex("(list 1 2)").result, 'can evaluate a simple list');

        deepEqual([list([num(11)])], ex(e1).result, 'can evaluate a simple list while ignoring whitespace');

        deepEqual([list([num(3)])], ex(e2).result, 'can evaluate a nested list');

        deepEqual([str('blargh')], ex(e3).result, 'can evaluate a deeply nested list');

        deepEqual([num(3)], ex(e4).result, 'can ignore comments');

        var m = true;
        try {
            ex(e5);
            m = false;
        } catch(e) {};
        ok(m, 'strings and symbols must be separated by a space');
    });
    
    
    test("multiple expressions", function() {
        expect(2);

        var e1 = "(define x 3) (cons x (list))"
            ;

        var r = beagle.exec(e1);
        deepEqual(list([num(3)]), r.result[1], 'multiple expressions can be evaluated at once');
        deepEqual(data.Nil(), r.result[0], "and the return value of 'define' is nil");
    });


    test("parseString", function() {
        expect(9);
        var p1 = "(+ 3 2) ; a comment",
            p2 = "3 4",
            p3 = '(+ (- (* 4 "blargh" 17)))',
            p4 = '(+ (- (* 4 "blargh" 17))',
            p5 = "()",
            p6 = "abc",
            p7 = "",
            p8 = "(define x 4) \n (define y 5)",
            p9 = "abc 1 2 3 (duh)",
            sexpr = parse.SExpression;

      var a = beagle.parseString(p1);
      deepEqual(
          [sexpr('list', [sexpr('symbol', '+'), sexpr('symbol', '3'), sexpr('symbol', '2')])],
          a, 'a simple list of three elements, all symbols; whitespace and comments are tossed'
      );

      var b = beagle.parseString(p2);
      deepEqual([sexpr("symbol", '3'), sexpr('symbol', '4')], b, 'multiple atoms are fine');

      var c = beagle.parseString(p3);
      deepEqual([
          sexpr("list", [
              sexpr('symbol', '+'), 
              sexpr('list', [
                  sexpr('symbol', '-'),
                  sexpr('list', [
                      sexpr('symbol', '*'),
                      sexpr('symbol', '4'),
                      sexpr('string', 'blargh'),
                      sexpr('symbol', '17'),
                  ])
              ])
          ])], c, 'deeply nested lists are no problem ...');

      var d = false;
      try {
        beagle.parseString(p4);
      } catch(e) {
        d = true;
      };
      ok(d, "... as long as they don't have unbalanced parentheses");

      var e = beagle.parseString(p5);
      deepEqual([sexpr('list', [])], e, 'the empty list is easy to parse');

      var f = beagle.parseString(p6);
      deepEqual([sexpr('symbol', 'abc')], f, 'and so is a lone symbol');
  
      var g = beagle.parseString(p7);
      deepEqual(g, [], "the empty string can be parsed but doesn't return anything");

      deepEqual([
          sexpr('list', [
              sexpr('symbol', 'define'),
              sexpr('symbol', 'x'),
              sexpr('symbol', '4')
          ]),
          sexpr('list', [
              sexpr('symbol', 'define'),
              sexpr('symbol', 'y'),
              sexpr('symbol', '5')
          ])
      ], beagle.parseString(p8), 'multiple sexpressions are fine');
            
      deepEqual([
          sexpr('symbol', 'abc'),
          sexpr('symbol', '1'),
          sexpr('symbol', '2'),
          sexpr('symbol', '3'),
          sexpr('list', [sexpr('symbol', 'duh')])       
      ], beagle.parseString(p9), "even if there's lots of them!");

    });

}
