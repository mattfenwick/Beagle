
function testFunctions(funcs, data, testHelper) {

    if( !funcs || !data ) {
      throw new Error("can't run tests without dependencies");
    }
    

    module("functions");
    
    var list = data.List,
        str = data.String,
        sym = data.Symbol,
        empty = list([]),
        num = data.Number,
        expectException = testHelper.expectException;


    test("cons", function() {
        var cons = funcs.cons,
            oneEl = cons([num(14), empty]),
            twoEl = cons([num(32), oneEl]),
            oneLi = cons([empty, empty]);
        
      deepEqual(list([num(14)]), oneEl, 'an element consed onto the empty list returns a one-element list');
      
      deepEqual(list([num(32), num(14)]), twoEl, 'an element consed onto THAT returns a two-element list');
      
      deepEqual(list([]), empty, 'cons does not mutate:  it makes a new list');
      
      deepEqual(list([empty]), oneLi, 'the first argument may be of any type, including a list');
      
      expectException(function() {
          cons([num(11), num(12)]);
      }, 'TypeError', 'the second argument must be a Beagle list');
      
      expectException(function() {
          cons([num(11)]);
      }, 'NumArgsError', 'too few arguments throws an exception ...');
      
      expectException(function() {
          cons([num(3), empty, empty]);
      }, 'NumArgsError', 'too many arguments is also a problem');
    });


    test("car", function() {
    
      var car = funcs.car,
          twoEl = list([3, 4]),
          listFirst = list([list([14])]);
      
      deepEqual(3, car([twoEl]), 'car returns the first element of a list, ');
      
      deepEqual(list([14]), car([listFirst]), 'which may be a list');

      expectException(function() {
          car([empty]);
      }, 'ValueError', 'trying to take the car of an empty list throws an exception');
      
      expectException(function() {
          car([num(16)]);
      }, 'TypeError', "car's argument must be a list");
      
      expectException(function() {
          car([]);
      }, 'NumArgsError', 'too few arguments throws an exception ...');
      
      expectException(function() {
          car([twoEl, twoEl]);
      }, 'NumArgsError', 'too many arguments is also a problem');
    });
      

    test("cdr", function() {
    
      var cdr = funcs.cdr,
          fourEl = list([3, 4, 10, 'hello']),
          oneEl = list([64]);

      deepEqual(
          list([4, 10, 'hello']), 
          cdr([fourEl]),
          "cdr returns the 'rest' of a list after the first element"
      );
      
      deepEqual(empty, cdr([oneEl]), 'the cdr of a one-element list is an empty list');

      expectException(function() {
          cdr([empty]);
      }, 'ValueError', 'trying to take the cdr of an empty list throws an exception');
      
      expectException(function() {
          cdr([num(16)]);
        }, 'TypeError', "cdr's argument must be a list");
      
      expectException(function() {
          cdr([]);
      }, 'NumArgsError', 'too few arguments throws an exception ...');
      
      expectException(function() {
          cdr([oneEl, oneEl]);
      }, 'NumArgsError', 'too many arguments is also a problem');

    });


    test("list", function() {
      
      var listf = funcs.list;
      deepEqual(list([3, 4, 5]), listf([3, 4, 5]), "'list' is a variadic function which returns its arguments in a list");
      
    });


    test("+", function() {

      var plus = funcs['+'];

      deepEqual(num(32), plus([num(27), num(5)]), "'+' is for adding two numbers");

      deepEqual(num(14), plus([num(18), num(-4)]), "they can be positive or negative");

      deepEqual(num(-17), plus([num(-9), num(-8)]), "or both negative");

      expectException(function() {
          plus([list([]), num(4)]);
      }, 'TypeError', 'both the first argument ...');
      
      expectException(function() {
          plus([num(8), list([])]);
        }, 'TypeError', "and the second argument must be numbers");
      
      expectException(function() {
          plus([num(4)]);
      }, 'NumArgsError', 'too few arguments throws an exception ...');
      
      expectException(function() {
          plus([num(4), num(5), num(6)]);
      }, 'NumArgsError', 'as does too many arguments');

    });


    test("neg", function() {
      var neg = funcs.neg,
          p3 = num(3),
          m3 = num(-3),
          m14 = num(-14);

      deepEqual(m3, neg([p3]), "'neg' negates a number, flipping the sign");

      deepEqual(m14, neg([neg([m14])]), "a number is its own double negative");
      
      expectException(function() {
          neg([list([])]);
        }, 'TypeError', "the first argument must be a number");
      
      expectException(function() {
          neg([]);
      }, 'NumArgsError', 'too few arguments throws an exception ...');
      
      expectException(function() {
          neg([num(4), num(5)]);
      }, 'NumArgsError', 'as does too many arguments');
    });


    test("equals", function() {
      
      var eq = funcs['='],
          db = Data.Boolean,
          t = db(true),
          f = db(false);

      deepEqual(t, eq([t, t]), 'booleans');
      deepEqual(f, eq([f, t]), 'booleans');
      
      deepEqual(t, eq([num(31), num(31)]), 'number');
      deepEqual(f, eq([num(3), num(31)]), 'number');
      deepEqual(t, eq([num(2331), num(2331)]), 'number');
      
      deepEqual(t, eq([str("xyz"), str("xyz")]), 'strings');
      deepEqual(f, eq([str("yz"), str("xyz")]), 'strings');
      
      deepEqual(t, eq([sym('abc'), sym('abc')]), 'symbols');
      deepEqual(f, eq([sym('abc'), sym('def')]), 'symbols');
      
      deepEqual(Data.Nil(), eq([sym('abc'), str('abc')]), 'mixed types');
      deepEqual(Data.Nil(), eq([num(16), db(true)]), 'mixed types');
      
      deepEqual(t, eq([list([]), list([])]), 'empty lists');
      deepEqual(f, eq([list([]), list([num(3)])]), 'empty and non-empty lists');
      deepEqual(t, eq([list([num(3)]), list([num(3)])]), '2 non-empty lists');
      deepEqual(f, eq([list([list([t])]), list([list([f])])]), 'deeply nested lists');
      deepEqual(f, eq([list([num(3)]), list([str('3')])]), 'empty and non-empty lists');
      deepEqual(
          f, 
          eq([list([num(3), list([])]), list([str('3'), list([])])]), 
          'nested lists'
      );
      
      expectException(function() {
          eq([num(7)]);
      }, 'NumArgsError', 'too few arguments throws an exception ...');
      
      expectException(function() {
          eq([num(4), num(5), num(8)]);
      }, 'NumArgsError', 'as does too many arguments');
      
    });
    
    
    test("type", function() {
        var type = funcs.type;

        deepEqual(str("number"), type([num(14)]), "'type' is a function of one argument");
        
        deepEqual(str("list"), type([list([])]), "it returns the type of its argument as a string");
        
        expectException(function() {
            type([]);
        }, 'NumArgsError', 'too few arguments throws an exception ...');
        
        expectException(function() {
            type([num(5), num(8)]);
        }, 'NumArgsError', 'as does too many arguments');
        
    });
    
}