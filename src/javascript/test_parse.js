
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
    	expect(15);

      var open = "  ((duh",
          close = ") what's going on",
          symbol = " abc)(()",
          number = " 12345 )",
          empty = "",
          str = ' "abc" ',
          wsstr = ' "ab cd" f';

      var o = lang.nextToken(open);
      equal('(', o.token.value);
      equal('open', o.token.type);
      equal('(duh', o.rest);

      var c = lang.nextToken(close);
      equal(')', c.token.value);
      equal('close', c.token.type);
      equal(" what's going on", c.rest);

      var s = lang.nextToken(symbol);
      equal('abc', s.token.value);
      equal('symbol', s.token.type);
      equal(')(()', s.rest);

      var n = lang.nextToken(number);
      equal('12345', n.token.value);
      equal('symbol', n.token.type);
      equal(' )', n.rest);

      var p = lang.nextToken(empty);
      equal(false, p, "empty string");
      
      var q = lang.nextToken(str);
      deepEqual([q.token.type, q.token.value, q.rest], ['string', 'abc', '']);
      
      var r = lang.nextToken(wsstr);
      deepEqual([r.token.type, r.token.value, r.rest], ['string', 'ab cd', " f"]);
    });

    test("tokenize", function() {
      var s1 = "  (abc)",
          s2 = "(((((((  ",
          s3 = "))) \t)\n(((",
          s4 = "abc123    abc(der)",
          s5 = "(+ 1 2 (+ 3 4 (+ 5 (/ 6 7))))",
          s6 = "";

      var getVal = function (t) {return t.value;};

      deepEqual(["(", "abc", ")"], lang.tokenize(s1).map(getVal));

      deepEqual(["(", "(", "(", "(", "(", "(", "("], lang.tokenize(s2).map(getVal));

      deepEqual([")", ")", ")", ")", "(", "(", "("], lang.tokenize(s3).map(getVal));

      deepEqual(["abc123", "abc", "(", "der", ")"], lang.tokenize(s4).map(getVal));

      deepEqual(["(", "+", "1", "2", "(", "+", "3", "4",
                 "(", "+", "5", "(", "/", "6", "7", ")",
                 ")", ")", ")"], 
          lang.tokenize(s5).map(getVal)
      );

      deepEqual([], lang.tokenize(s6));

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

      var o = lang.getList(t1);
      equal(false, o, "list needs an open as well as a close");

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


    test("parse", function() {
      expect(9);
      var p1 = "(+ 3 2)",
          p2 = "3 4",
          p3 = "(+ (- (* 4 5 (abc (quote def)) 17)))",
          p4 = "(+ (- (* 4 5 (abc (quote def)) 17))",
          p5 = "()",
          p6 = "abc",
          p7 = "",
          p8 = "(define x 4) \n (define y 5)",
          p9 = "abc 1 2 3 (duh)";

      var a = lang.parse(p1);
      deepEqual([
        {
          "type": "list",
          "value": [
            {
              "type": "symbol",
              "value": "+"
            },
            {
              "type": "symbol",
              "value": "3"
            },
            {
              "type": "symbol",
              "value": "2"
            }
          ]
        }
      ], asJson(a));

      var b = lang.parse(p2);
      deepEqual([{
            "type": "symbol",
            "value": "3"
          },
          {
            "type": "symbol",
            "value": "4"
          }
        ], asJson(b));

      var c = lang.parse(p3);
      deepEqual([
  {
    "type": "list",
    "value": [
      {
        "type": "symbol",
        "value": "+"
      },
      {
        "type": "list",
        "value": [
          {
            "type": "symbol",
            "value": "-"
          },
          {
            "type": "list",
            "value": [
              {
                "type": "symbol",
                "value": "*"
              },
              {
                "type": "symbol",
                "value": "4"
              },
              {
                "type": "symbol",
                "value": "5"
              },
              {
                "type": "list",
                "value": [
                  {
                    "type": "symbol",
                    "value": "abc"
                  },
                  {
                    "type": "list",
                    "value": [
                      {
                        "type": "symbol",
                        "value": "quote"
                      },
                      {
                        "type": "symbol",
                        "value": "def"
                      }
                    ]
                  }
                ]
              },
              {
                "type": "symbol",
                "value": "17"
              }
            ]
          }
        ]
      }
    ]
  }
], asJson(c));

      var d = false;
      try {
        lang.parse(p4);
      } catch(e) {
        d = true;
      };
      ok(d, "unbalanced parentheses cause parse to fail");

      var e = lang.parse(p5);
      deepEqual([{
        "type": "list",
        "value": []
      }], asJson(e));

      var f = lang.parse(p6);
      deepEqual([{
        "type": "symbol",
        "value": "abc"
      }], asJson(f));
  
      var g = lang.parse(p7);
      deepEqual(g, [], "empty string results in empty list");

      deepEqual(	

[
  {
    "type": "list",
    "value": [
      {
        "type": "symbol",
        "value": "define"
      },
      {
        "type": "symbol",
        "value": "x"
      },
      {
        "type": "symbol",
        "value": "4"
      }
    ]
  },
  {
    "type": "list",
    "value": [
      {
        "type": "symbol",
        "value": "define"
      },
      {
        "type": "symbol",
        "value": "y"
      },
      {
        "type": "symbol",
        "value": "5"
      }
    ]
  }
], asJson(lang.parse(p8)));
            
      deepEqual([
  {
    "type": "symbol",
    "value": "abc"
  },
  {
    "type": "symbol",
    "value": "1"
  },
  {
    "type": "symbol",
    "value": "2"
  },
  {
    "type": "symbol",
    "value": "3"
  },
  {
    "type": "list",
    "value": [
      {
        "type": "symbol",
        "value": "duh"
      }
    ]
  }
], asJson(lang.parse(p9)));

    });

}
