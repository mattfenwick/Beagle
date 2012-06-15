
function testParse(lang) {

    module("tokenizer");
    
    test("nextToken", function() {
    	expect(11);

      var open = "  ((duh",
          close = ") what's going on",
          symbol = " abc)(()",
          number = " 12345 )",
          empty = "",
          str = ' "abc" ',
          wsstr = ' "ab cd" f';

      var o = lang.nextToken(open);
      equal('(', o.token);
      equal('(duh', o.rest);

      var c = lang.nextToken(close);
      equal(')', c.token);
      equal(" what's going on", c.rest);

      var s = lang.nextToken(symbol);
      equal('abc', s.token);
      equal(')(()', s.rest);

      var n = lang.nextToken(number);
      equal('12345', n.token);
      equal(' )', n.rest);

      var p = lang.nextToken(empty);
      deepEqual({token: false, rest: false}, p);
      
      deepEqual({token: '"abc"', rest: ""}, lang.nextToken(str));
      
      deepEqual({token: '"ab cd"', rest: "f"}, lang.nextToken(wsstr));
    });

    test("tokenize", function() {
      var s1 = "  (abc)",
          s2 = "(((((((  ",
          s3 = "))) \t)\n(((",
          s4 = "abc123    abc(der)",
          s5 = "(+ 1 2 (+ 3 4 (+ 5 (/ 6 7))))",
          s6 = "";

      deepEqual(["(", "abc", ")"], lang.tokenize(s1));

      deepEqual(["(", "(", "(", "(", "(", "(", "("], lang.tokenize(s2));

      deepEqual([")", ")", ")", ")", "(", "(", "("], lang.tokenize(s3));

      deepEqual(["abc123", "abc", "(", "der", ")"], lang.tokenize(s4));

      deepEqual(["(", "+", "1", "2", "(", "+", "3", "4",
                 "(", "+", "5", "(", "/", "6", "7", ")",
                 ")", ")", ")"], 
          lang.tokenize(s5)
      );

      deepEqual([], lang.tokenize(s6));

    });    
	  

    module("parse");
    
    test("getSymbol", function() {

      var t1 = ["(", "duh"],
          t2 = ["123abc", "(", 'barf'],
          t3 = [")", "abc", "("],
          t4 = [];

      var o = lang.getSymbol(t1);
      equal(false, o, "( is not a symbol");

      var p = lang.getSymbol(t2);
      equal("123abc", p.result, "symbol");
      deepEqual(["(", 'barf'], p.rest, "rest of token stream");

      var q = lang.getSymbol(t3);
      equal(false, q, ") is not a symbol");
      
      equal(false, lang.getSymbol(t4), "can't find symbol empty list");
    });

    test("getList", function() {
      expect(12);
      var t1 = [")"],
          t2 = ["(", ")"],
          t3 = ["(", "abc", ")", "def"],
          t4 = ["(", "1", "(", "+", ")", "2", ")", "(", "rest!"],
          t5 = ["abc"],
          t6 = [],
          t7 = ["(", "abc", "(", "-", ")", "ohnoes"],
          t8 = ["(", "(", "(", "(", ")", ")", ")", ")", "12345"],
          t9 = ["(", "(", "(", "(", ")", ")", ")"];

      var o = lang.getList(t1);
      equal(false, o);

      var p = lang.getList(t2);
      deepEqual([], p.result);

      var q = lang.getList(t3);
      deepEqual(["abc"], q.result);
      deepEqual(["def"], q.rest);

      var r = lang.getList(t4);
      deepEqual(["1", ["+"], "2"], r.result);
      deepEqual(["(", "rest!"], r.rest);

      var s = lang.getList(t5);
      equal(false, s);

      var t = lang.getList(t6);
      equal(false, t);

      var u = lang.getList(t7);
      equal(false, u);
      
      var v = lang.getList(t8);
      deepEqual([[[[]]]], v.result);
      deepEqual(["12345"], v.rest);
      
      var w = lang.getList(t9);
      equal(false, w);
    });

    test("parseOne", function() {
      expect(7);
      var p1 = "(+ 3 2)",
          p2 = "3 4",
          p3 = "(+ (- (* 4 5 (abc (quote def)) 17)))",
          p4 = "(+ (- (* 4 5 (abc (quote def)) 17))",
          p5 = "()",
          p6 = "abc",
          p7 = "";

      var a = lang.parse(p1);
      deepEqual(["+", "3", "2"], a);

      var b = lang.parse(p2);
      equal(false, b);

      var c = lang.parse(p3);
      deepEqual([
        "+", 
         ["-", 
          ["*", "4", "5", 
           ["abc", 
            ["quote", "def"]], 
           "17"]]], 
      c);

      var d = lang.parse(p4);
      equal(false, d);

      var e = lang.parse(p5);
      deepEqual([], e);

      var f = lang.parse(p6);
      equal("abc", f);

      var g = lang.parse(p7);
      equal(false, g);

    });

    test("parseAll", function() {
      expect(7);
      var p1 = "(+ 3 2)",
          p2 = "3 4",
          p3 = "(+ (- (* 4 5 \t 17)))",
          p4 = "(define x 4) \n (define y 5)",
          p5 = "()",
          p6 = "abc 1 2 3 (duh)",
          p7 = "";
      
      deepEqual({'sexprs':[["+", "3", "2"]]}, lang.parseAll(p1));
      
      deepEqual({'sexprs':["3", "4"]}, lang.parseAll(p2));
      
      deepEqual({'sexprs':[["+", ["-", ["*", "4", "5", "17"]]]]}, lang.parseAll(p3));
      
      deepEqual({'sexprs':[["define", "x", "4"], ["define", "y", "5"]]}, lang.parseAll(p4));
      
      deepEqual({'sexprs':[[]]}, lang.parseAll(p5));
      
      deepEqual({'sexprs':["abc", "1", "2", "3", ["duh"]]}, lang.parseAll(p6));
      
      deepEqual({'sexprs':[]}, lang.parseAll(p7));
      
    });

}

