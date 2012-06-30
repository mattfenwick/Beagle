
function testParse(lang) {


    module("tokenizer");
    
    test("nextToken", function() {
    	expect(11);

      var open = "((duh",
          close = ") what's going on",
          symbol = "abc)(()",
          number = "12345 )",
          empty = "",
          str = '"abc" ',
          wsstr = '"ab cd" f',
          semsym = 'wh;at',
          comm = '; this is\nhi',
          ws = '\n\thello\n',
          badstring = '"abc';

      var p = lang.nextToken(empty);
      ok(p === false, "empty string:  no token found, false returned");

      var o = lang.nextToken(open);
      deepEqual({
        'rest': '(duh', 
        'token': lang.Token('open', '(')
      }, o, "single '(': open token");

      var c = lang.nextToken(close);
      deepEqual({
        'rest': " what's going on",
        'token': lang.Token('close', ')')
      }, c, "single ')': close token");

      var s = lang.nextToken(symbol);
      deepEqual({
        'rest': ')(()',
        'token': lang.Token('symbol', 'abc')
      }, s, "all letters:  symbol token");

      var n = lang.nextToken(number);
      deepEqual({
        'rest': ' )',
        'token': lang.Token('symbol', '12345')
      }, n, "all numbers:  symbol token");
      
      var q = lang.nextToken(str);
      deepEqual({
        'token': lang.Token('string', 'abc'), 
        'rest': ' '
      }, q, 'enclosed in " marks:  string token');
      
      var r = lang.nextToken(wsstr);
      deepEqual({
        'token': lang.Token('string', 'ab cd'),
        'rest': ' f'
      }, r, 'string tokens can contain whitespace');

      var zz = lang.nextToken(semsym);
      deepEqual({
        "rest": ";at", 
        "token": lang.Token('symbol', 'wh')
      }, zz, "symbols may not include ;'s");
      
      deepEqual({
        'rest': '\nhi',
        'token': lang.Token('comment', ' this is')
      }, lang.nextToken(comm), "comments begin with a ; and end at the next newline (\\n)");
      
      deepEqual({
        'rest': 'hello\n',
        'token': lang.Token('whitespace', '\n\t')
      }, lang.nextToken(ws), "whitespace (\\s in a regex) is also a token");
      
      var bsb = true;
      try {
        lang.nextToken(badstring);
        bsb = false;
      } catch(e) {};
      ok(bsb, 'a " (start string) without a matching " (close string) throws an exception');
    });
    

    test("tokenize", function() {
      var s1 = "  \t\n \t(abc)",
          s2 = "(((((((\n  ",
          s3 = "))) \t)\n(((",
          s4 = "abc123    abc(der)",
          s5 = "(+ 1 1 (+ 1 1 (+ 1 (+ 1 1))))",
          s6 = "";

      deepEqual([
        lang.Token('whitespace', "  \t\n \t"), 
        lang.Token('open', "("), 
        lang.Token('symbol', "abc"),
        lang.Token('close', ")")
      ], lang.tokenize(s1), "all consecutive whitespace is collapsed into a single token");

      var open = lang.Token('open', "(");
      deepEqual([
        open, open, open, open, open, open, open, lang.Token('whitespace', "\n  ")
      ], lang.tokenize(s2), "adjacent '('s are separate tokens");

      var close = lang.Token('close', ')');
      deepEqual([
        close, close, close, lang.Token('whitespace', " \t"), close, lang.Token('whitespace', "\n"), open, open, open
      ], lang.tokenize(s3), "lots of ')'s, '('s, and whitespace");

      deepEqual([
        lang.Token('symbol', "abc123"), lang.Token('whitespace', "    "), 
        lang.Token('symbol', "abc"), open, lang.Token('symbol', "der"), close
      ], lang.tokenize(s4), "symbols are terminated by '(' and ')'");

      var o = lang.Token('symbol', '1'),
          p = lang.Token('symbol', '+'),
          s = lang.Token('whitespace', ' ');
      deepEqual([
        open, p, s, o, s, o, s, open, p, s, o, s, o, s, 
        open, p, s, o, s, open, p, s, o, s, o, close,
        close, close, close
      ], lang.tokenize(s5), "lots of nested lists");

      deepEqual([], lang.tokenize(s6), "an empty string yields an empty list");

    });
    

    test("strip comments and whitespace", function() {

      var t1 = [
        lang.Token('comment', 'abc'), 
        lang.Token('string', 'derrrr'), 
        lang.Token('whitespace', '\t\n     \n'), 
        lang.Token('open', '('),
        lang.Token('whitespace', '   \t\t\t\n\t')
      ]; 

      var t2 = lang.stripTokens(t1);

      deepEqual([t1[1], t1[3]], t2, 'all whitespace and comment tokens are discarded by stripping');

    });  
	  

    module("parse");
    
    test("getAtom", function() {

      var open = lang.Token('open', '('),
          close = lang.Token('close', ')');

      var t1 = [open, lang.Token('symbol', "duh")],
          t2 = [
            lang.Token('symbol', "123abc"), 
            open,
            lang.Token('symbol', 'barf')
          ],
          t3 = [close, "abc", open],
          com = [lang.Token('comment', 'blargh')],
          str = [lang.Token('string', 'me!')],
          ws = [lang.Token('whitespace', '\n \t')];

      var o = lang.getAtom(t1);
      equal(false, o, "'(' is not an atom");
      
      equal(false, lang.getAtom(t3), " ... nor is ')' ...");
      
      equal(false, lang.getAtom(com), " ... nor is a comment ...");
      
      equal(false, lang.getAtom(ws), " ... nor is whitespace");

      var p = lang.getAtom(t2);
      deepEqual({
        'result': lang.SExpression('symbol', "123abc"), 
        'rest': t2.slice(1)
      }, p, "however, symbols ARE atoms ...");

      var z = lang.getAtom(str);
      deepEqual({
        'result': lang.SExpression('string', 'me!'),
        'rest': []
      }, z, "... and so are strings");
                  
      equal(false, lang.getAtom([]), "and don't forget:  there's no atoms in an empty list");

    });



    test("getList", function() {
      expect(11);
      function m(t) {
          var o = {'value': t};
          if( t === '(' )      {o.type = 'open'; }
          else if( t === ')' ) {o.type = 'close';}
          else { o.type = 'symbol';}
          return o;
      }

      var t0 = [],
          t1 = ["("].map(m),
          t2 = [")"].map(m),
          t3 = ["(", ")"].map(m),
          t4 = ["abc"].map(m),
          t5 = ["(", "abc", ")", "def"].map(m),
          t6 = ["(", "1", "(", "+", ")", "2", ")", "(", "rest!"].map(m),
          t7 = ["(", "abc", "(", "-", ")", "ohnoes"].map(m),
          t8 = ["(", "(", "(", "(", ")", ")", ")", ")", "12345"].map(m),
          t9 = ["(", "(", "(", "(", ")", ")", ")"].map(m);


      var t = lang.getList(t0);
      equal(false, t, "trying to get a list from an empty token stream returns false ... ");

      var a = true;
      try {
        lang.getList(t1);
        a = false;
      } catch(e) {}
      ok(a, "... while a '(' without a matching ')' throws an error,");

      var b = true;
      try {
        lang.getList(t2);
        b = false;
      } catch(e) {}
      ok(b, "and a leading ')' also throws an error");

      var p = lang.getList(t3);
      deepEqual({
        result: lang.SExpression('list', []),
        rest: []
      }, p, "() is parsed as an empty list");

      var s = lang.getList(t4);
      equal(false, s, "you can't get a list from a symbol ...");
      
      equal(false, lang.getList(['"123"', '+'].map(m)), " ... or from a string");

      var q = lang.getList(t5);
      deepEqual({
        'result': lang.SExpression('list', [lang.SExpression('symbol', 'abc')]), 
        'rest':  t5.slice(3)
      }, q, "a list extends from the opening '(' to the next ')', unless ...");

      var r = lang.getList(t6);
      deepEqual({
        'result': lang.SExpression('list', [
            lang.SExpression('symbol', '1'),
            lang.SExpression('list', [lang.SExpression('symbol', '+')]),
            lang.SExpression('symbol', '2')
        ]),
        'rest': t6.slice(7)
      }, r, "... it has a nested list, in which case it extends to its 'balancing' ')'");

      var u = true;
      try {
        lang.getList(t7);
        u = false;
      } catch(e) {};
      ok(u, "therefore: a missing 'balancing' ')' throws an error");
      
      var v = lang.getList(t8);
      deepEqual({
        'result': lang.SExpression('list',
          [lang.SExpression('list',
            [lang.SExpression('list',
              [lang.SExpression('list', [])])])]),
        'rest': t8.slice(8)
      }, v, "lists may be arbitrarily deeply nested ...");
      
      var w = true;
      try {
        lang.getList(t9);
        w = false;
      } catch(e) {};
      ok(w, "... as long as the parentheses match");
    });
    
    

    test("check token separation", function() {

      var tokens = [lang.Token('symbol', 'abc'), lang.Token('comment', 'nope'),
                    lang.Token('whitespace', '   '), lang.Token('open', '('),
                    lang.Token('close', ')'), lang.Token('string', 'hahaha')];

      var types = {'string': 1, 'symbol': 1};

      var i, j, passed, myTokens;
      for(i = 0; i < tokens.length; i++) {
        for(j = 0; j < tokens.length; j++) {
          passed = true;
          try {
            myTokens = [tokens[i], tokens[j]];
            lang.checkTokenSeparation(myTokens);
            passed = true;
          } catch(e) {
            passed = false;
          };
          if( types[tokens[i].type] && types[tokens[j].type] ) {
            ok(!passed, 'consecutive strings/symbols throws an exception');
          } else {
            ok(passed, "no problem for tokens " + JSON.stringify(myTokens));
          }
        }
      }
    });
}
