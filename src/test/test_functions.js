
function testFunctions(funcs, data, testHelper) {

    if( !funcs || !data ) {
      throw new Error("can't run tests without dependencies");
    }
    

    module("functions");
    
    var list = data.List,
        ch = data.Char,
        str = data.String,
        sym = data.Symbol,
        empty = list([]),
        num = data.Number,
        expectException = testHelper.expectException;


    test("cons", function() {
        var cons = funcs.cons,
            oneEl = cons([num(14), empty]),
            twoEl = cons([num(32), oneEl]),
            oneLi = cons([empty, empty]),
            str1 = str("eat"),
            ch1 = ch("f");
        
      deepEqual(list([num(14)]), oneEl, 'an element consed onto the empty list returns a one-element list');
      
      deepEqual(list([num(32), num(14)]), twoEl, 'an element consed onto THAT returns a two-element list');
      
      deepEqual(list([]), empty, 'cons does not mutate:  it makes a new list');
      
      deepEqual(list([empty]), oneLi, 'if the 2nd arg is a list, the 1st arg may be of any type ...');
      
      deepEqual(str("feat"), cons([ch1, str1]), 'but if the 2nd arg is a string, the 1st arg must be a char');
      
      expectException(function() {
    	  cons([num(11), str1]);
      }, 'TypeError', "otherwise, you'll get an error")
      
      expectException(function() {
          cons([num(11), num(12)]);
      }, 'TypeError', 'the second argument must be a Beagle list or string');
    });


    test("car", function() {
    
      var car = funcs.car,
          twoEl = list([3, 4]),
          listFirst = list([list([14])]);
      
      deepEqual(3, car([twoEl]), 'car returns the first element of a list or string, ');
      
      deepEqual(list([14]), car([listFirst]), 'which may be a list');
      
      deepEqual(ch('x'), car([str('xylophone')]), 'the car of a string is a character');

      expectException(function() {
          car([empty]);
      }, 'ValueError', 'trying to take the car of an empty list throws an exception');

      expectException(function() {
          car([str("")]);
      }, 'ValueError', 'as does taking the car of an empty string');
      
      expectException(function() {
          car([num(16)]);
      }, 'TypeError', "car's argument must be a list/string");
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
      
      deepEqual(str("hark"), cdr([str("shark")]), 'the cdr of a string is another string, missing the first character');

      expectException(function() {
          cdr([empty]);
      }, 'ValueError', 'trying to take the cdr of an empty list throws an exception');
      
      expectException(function() {
    	  cdr([str("")]);
      }, 'ValueError', 'as does taking the cdr of an empty string');
      
      expectException(function() {
          cdr([num(16)]);
        }, 'TypeError', "cdr needs a list or string");

    });
    
    
    test("null?", function() {
        var n = funcs['null?'],
            empty = list([]);

        deepEqual(data.Boolean(true), n([empty]), "'null?' takes one argument:  a list/string");
        
        deepEqual(data.Boolean(false), n([list([str("list")])]), "it returns true if the list is empty, and false otherwise");

        deepEqual(data.Boolean(true), n([str("")]), "and similarly for strings, the empty string is true");
        
        deepEqual(data.Boolean(false), n([str("a")]), "any other string is false");

        expectException(function() {
            n([num(4)]);
        }, 'TypeError', 'remember to give it a list or string');
        
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
    });


    test("eq?", function() {
      
      var eq = funcs['eq?'],
          db = Data.Boolean,
          t = db(true),
          f = db(false),
          ch1 = ch('x'),
          ch2 = ch('y');

      deepEqual(t, eq([t, t]), 'booleans');
      deepEqual(f, eq([f, t]), 'booleans');
      

      deepEqual(f, eq([num(3), num(31)]), 'number');
      deepEqual(t, eq([num(2331), num(2331)]), 'number');
      
      deepEqual(t, eq([ch1, ch1]), 'chars');
      deepEqual(f, eq([ch1, ch2]), 'chars');
      
      deepEqual(t, eq([str("abc"), str("abc")]), 'strings');
      deepEqual(f, eq([str("def"), str("defg")]), 'strings');
      
      deepEqual(t, eq([sym('abc'), sym('abc')]), 'symbols');
      deepEqual(f, eq([sym('abc'), sym('def')]), 'symbols');
      
      expectException(function() {
          eq([num(16), db(true)]);
      }, 'TypeError', "'eq?' arguments must be of the same type");
            
      expectException(function() {
          eq([list(7), list(7)]);
      }, 'TypeError', "'eq?' does not work on lists");
      
    });
    
    
    test("prim-type", function() {
        var type = funcs['prim-type'],
            str1 = str('number'),
            str2 = str('list');

        deepEqual(str1, type([num(14)]), "'prim-type' is a function of one argument");
        
        deepEqual(str2, type([list([])]), "it returns the type of its argument as a string");
        
    });
    
    
    test("number-<", function() {
        var lt = funcs['number-<'];

        deepEqual(data.Boolean(false), lt([num(2), num(1)]), "'number-<' takes two numbers and compares them");
        
        deepEqual(data.Boolean(true), lt([num(11), num(39)]), "it returns true if the first is < the second");
        
        deepEqual(data.Boolean(false), lt([num(4), num(4)]), "and false otherwise -- including if they're the same");

        expectException(function() {
            lt([list([]), num(4)]);
        }, 'TypeError', 'remember to give it numbers');
    });
    
    
    test("data, udt-type and udt-value", function() {
        var da = funcs['data'],
            con1 = da([str("obj")]),
            ut = funcs['udt-type'],
            uv = funcs['udt-value'],
            obj1 = con1.value([num(39)]);

        deepEqual(
            "function", 
            con1.type, 
            "'data' creates user-defined types, taking one string argument and returning a constructor function"
        );
        
        deepEqual(
            data.UserDefined(str("obj"), num(39)), 
            obj1, 
            "constructors take one argument, and return an object with the appropriate type"
        );

        expectException(function() {
            da([num(4)]);
        }, 'TypeError', "remember to give 'data' a string");
        
        
        deepEqual(str("obj"), ut([obj1]), "udt-type returns the type (as a string) of a user-defined datatype");
        
        expectException(function() {
            ut([num(13)]);
        }, 'TypeError', "udt-type only works on user-defined types");
        
        
        deepEqual(num(39), uv([obj1]), "'udt-value' returns the value of a user-defined datatype");
        
        expectException(function() {
            uv([num(13)]);
        }, 'TypeError', "'udt-value' also only works on user-defined types");
    });
    
    
    test("Error", function() {
    	var err = funcs['Error'];

        expectException(function() {
            err([list([]), num(4), num(3)]);
        }, 'TypeError', 'remember to give it strings for the first 2 args');
    });
    
    
    test("error-type", function() {
        var errtype = funcs['error-type'];

        expectException(function() {
            errtype([list([])]);
        }, 'TypeError', 'remember to give it an Error');
    });
    
    
    test("error-message", function() {
        var errmess = funcs['error-message'];

        expectException(function() {
            errmess([list([])]);
        }, 'TypeError', 'remember to give it an Error');
    });
    
    
    test("error-trace", function() {
        var errtrace = funcs['error-trace'];

        expectException(function() {
            errtrace([list([])]);
        }, 'TypeError', 'remember to give it an Error');
    });
    
    
    test("function number of arguments", function() {
    	
    	var args = {
    		'cons'         : 2,
    		'car'          : 1,
    		'cdr'          : 1,
    		'null?'        : 1, 
    		'+'            : 2,
    		'neg'          : 1,
    		'number-<'     : 2,
    		'eq?'          : 2,
    		'prim-type'    : 1,
    		'data'         : 1,
    		'udt-value'    : 1,
    		'udt-type'     : 1,
    		'error-trace'  : 1,
    		'error-message': 1,
    		'error-type'   : 1,
    		'Error'        : 3
    	}, 
    	n_args = [
    	    [],
    	    [str("abc")],
    	    [str("abc"), str("abc")],
    	    [str("abc"), str("abc"), str("abc")],
    	    [str("abc"), str("abc"), str("abc"), str("abc")]
    	];
    	
    	for(var fname in Functions) {
    		// because 'list' is variadic
    		if(fname === 'list') {
    			continue;
    		}
    		if(!(fname in args)) {
    			ok(false, "no entry for " + fname);
    		}
    		numArgs = args[fname];
    		expectException(function() {
    			Functions[fname](n_args[numArgs - 1]);
    		}, 'NumArgsError', 'function ' + fname + ': too few arguments');
    		expectException(function() {
    			Functions[fname](n_args[numArgs + 1]);
    		}, 'NumArgsError', 'function ' + fname + ': too many arguments');
    	}
    	
    });
    
}