
function testBeagle(beagle, data) {

    module("beagle");
    
    test("single expressions", function() {
    	expect(5);

      var e1 = " ( list ) ",
          e2 = "(cons 3 (list))",
          e3 = '(car (cdr (cons 1 (cons "blargh" (cons 3 (cons 4 (list)))))))',
          e4 = ' 3 ; this is a comment'
          e5 = '"abc"123';

      deepEqual([data.List([])], beagle.exec(e1).result);

      deepEqual([data.List([data.Number(3)])], beagle.exec(e2).result);

      deepEqual([data.String('blargh')], beagle.exec(e3).result);

      deepEqual([data.Number(3)], beagle.exec(e4).result);

      var m = true;
      try {
        beagle.parseString(e5);
        m = false;
      } catch(e) {};
      ok(m, 'strings and symbols must be separated by a space');
    });
    
    test("multiple expressions", function() {
    	expect(2);

      var e1 = "(define x 3) (cons x (list))"
          ;

      var r = beagle.exec(e1);
      deepEqual(data.Nil(), r.result[0]);
      deepEqual(data.List([data.Number(3)]), r.result[1]);
    });



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


    test("parseString", function() {
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

      var a = beagle.parseString(p1);
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

      var b = beagle.parseString(p2);
      deepEqual([{
            "type": "symbol",
            "value": "3"
          },
          {
            "type": "symbol",
            "value": "4"
          }
        ], asJson(b));

      var c = beagle.parseString(p3);
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
        beagle.parseString(p4);
      } catch(e) {
        d = true;
      };
      ok(d, "unbalanced parentheses cause parse to fail");

      var e = beagle.parseString(p5);
      deepEqual([{
        "type": "list",
        "value": []
      }], asJson(e));

      var f = beagle.parseString(p6);
      deepEqual([{
        "type": "symbol",
        "value": "abc"
      }], asJson(f));
  
      var g = beagle.parseString(p7);
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
], asJson(beagle.parseString(p8)));
            
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
], asJson(beagle.parseString(p9)));

    });

}
