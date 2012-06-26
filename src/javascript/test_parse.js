
function testParse(lang) {


function asJson(obj) {
  var out;
  if( typeof(obj) === 'object' && obj.length !== undefined ) {
    out = [];
    for(var i = 0; i < obj.length; i++) {
      out.push(asJson(obj[i]));
    }
  } else if( typeof(obj) === 'object' ) {
    out = {};
    for(var key in obj) {
      out[key] = asJson(obj[key]);
    }
  } else { // it's a number/string
    out = obj;
  }
  return out;
}


    module("tokenizer");
    
    test("nextToken", function() {
    	expect(10);

      var open = "((duh",
          close = ") what's going on",
          symbol = "abc)(()",
          number = "12345 )",
          empty = "",
          str = '"abc" ',
          wsstr = '"ab cd" f',
          semsym = 'wh;at',
          comm = '; this is\nhi',
          ws = '\n\thello\n';

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
    });
    

    test("tokenize", function() {
      var s1 = "  \t\n \t(abc)",
          s2 = "(((((((\n  ",
          s3 = "))) \t)\n(((",
          s4 = "abc123    abc(der)",
          s5 = "(+ 1 2 (+ 3 4 (+ 5 (/ 6 7))))",
          s6 = "";

      var getVal = function (t) {return t.value;};

      deepEqual(["  \t\n \t", "(", "abc", ")"], lang.tokenize(s1).map(getVal));

      deepEqual(["(", "(", "(", "(", "(", "(", "(", "\n  "], lang.tokenize(s2).map(getVal));

      deepEqual([")", ")", ")", " \t", ")", "\n", "(", "(", "("], lang.tokenize(s3).map(getVal));

      deepEqual(["abc123", "    ", "abc", "(", "der", ")"], lang.tokenize(s4).map(getVal));

      deepEqual(["(", "+", " ", "1", " ", "2", " ", "(", "+", " ", "3", " ", "4", " ", 
                 "(", "+", " ", "5", " ", "(", "/", " ", "6", " ", "7", ")",
                 ")", ")", ")"], 
          lang.tokenize(s5).map(getVal)
      );

      deepEqual([], lang.tokenize(s6));

    });

    test("strip comments and whitespace", function() {

      var t1 = [Parse.Token('comment', 'abc'), 
                Parse.Token('string', 'derrrr'), 
                Parse.Token('whitespace', '\t\n     \n'), 
                Parse.Token('open', '(')
      ]; 

      var t2 = Parse.stripTokens(t1);

      deepEqual(t2[0], Parse.Token('string', 'derrrr'), 'first element after stripping');

      equal(2, t2.length, 'tokens left after stripping');

    });  
	  

    module("parse");
    
    test("getAtom", function() {

      var t1 = [{'type': 'open', 'value': "("}, {'type': 'symbol', 'value': "duh"}],
          t2 = [{'type': 'symbol', 'value': "123abc"}, 
                {'type': 'open', 'value': "("},
                {'type': 'symbol', 'value': 'barf'}
          ],
          t3 = [
            {'type': 'close', 'value': ")"}, 
            "abc", 
            {'type': 'open', 'value': "("}
          ],
          t4 = [],
          com = [{'type': 'comment', 'value': 'blargh'}],
          str = [{'type': 'string', 'value': 'me!'}];

      var o = lang.getAtom(t1);
      equal(false, o, "( is not an atom");

      var p = lang.getAtom(t2);
      equal("123abc", p.result.value, "symbols are atoms");
      deepEqual(t2.slice(1), p.rest, "rest of token stream");

      var q = lang.getAtom(t3);
      equal(false, q, ") is not an atom");
      
      equal(false, lang.getAtom(t4), "no atom in empty list");

      equal(false, lang.getAtom(com), "comments are not atoms");

      var z = lang.getAtom(str).result;
      deepEqual(str[0], {'type': z.type, 'value': z.value}, "strings are atoms");
    });

    test("getList", function() {
      expect(13);
      function m(t) {
          var o = {'value': t};
          if( t === '(' )      {o.type = 'open'; }
          else if( t === ')' ) {o.type = 'close';}
          else { o.type = 'symbol';}
          return o;
      }

      var t1 = [")"].map(m),
          t2 = ["(", ")"].map(m),
          t3 = ["(", "abc", ")", "def"].map(m),
          t4 = ["(", "1", "(", "+", ")", "2", ")", "(", "rest!"].map(m),
          t5 = ["abc"].map(m),
          t6 = [],
          t7 = ["(", "abc", "(", "-", ")", "ohnoes"].map(m),
          t8 = ["(", "(", "(", "(", ")", ")", ")", ")", "12345"].map(m),
          t9 = ["(", "(", "(", "(", ")", ")", ")"].map(m);

      var getVal = function (t) {return t.value;};

      var o = true;
      try {
        lang.getList(t1);
        o = false;
      } catch(e) {}
      ok(o, "list needs an open as well as a close");

      var p = lang.getList(t2);
      deepEqual([], p.result.value);
      equal("list", p.result.type);

      var q = lang.getList(t3),
          q1 = q.result.value[0];
      deepEqual({'type': 'symbol', 'value': 'abc'}, {'type': q1.type, 'value': q1.value});
      deepEqual(["def"], q.rest.map(getVal));

      var r = lang.getList(t4),
          r1 = r.result.value;
// [{'type': 'symbol', 'value': "1"}, ["+"], "2"]
      deepEqual(['list', 'symbol', 'list', 'symbol'], [r.result.type, r1[0].type, r1[1].type, r1[1].value[0].type]);
      deepEqual(["(", "rest!"].map(m), r.rest);

      var s = lang.getList(t5);
      equal(false, s, "can't get list from symbol");

      var t = lang.getList(t6);
      equal(false, t, "can't get list from empty token stream");

      var u = true;
      try {
        lang.getList(t7);
        u = false;
      } catch(e) {
      };
      ok(u, "can't get list if missing close-paren");
      
      var v = lang.getList(t8);
      deepEqual({
          "type": "list",
          "value": [{
              "type": "list",
              "value": [{
                  "type": "list",
                  "value": [{
                      "type": "list",
                      "value": []
                    }
                  ]
                }
              ]
            }
          ]
        }, asJson(v.result));
      deepEqual([{'type': 'symbol', 'value': "12345"}], v.rest, "rest of deeply nested list");
      
      var w = true;
      try {
        lang.getList(t9);
        w = false;
      } catch(e) {};
      ok(w, "missing CLOSE in deeply nested list");
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
