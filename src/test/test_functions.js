
function testFunctions(funcs, data, testHelper) {

    if( !funcs || !data ) {
      throw new Error("can't run tests without dependencies");
    }
    

    module("functions");
    
    var list = data.List,
        ch = data.Char,
        str = data.makeCharList,
        sym = data.Symbol,
        empty = list([]),
        num = data.Number,
        expectException = testHelper.expectException;


    test("cons", function() {
        var cons = funcs.cons,
            oneEl = cons.fapply([num(14), empty]),
            twoEl = cons.fapply([num(32), oneEl]),
            oneLi = cons.fapply([empty, empty]),
            str1 = str("eat"),
            ch1 = ch("f");
        
      deepEqual(list([num(14)]), oneEl, 'an element consed onto the empty list returns a one-element list');
      
      deepEqual(list([num(32), num(14)]), twoEl, 'an element consed onto THAT returns a two-element list');
      
      deepEqual(list([]), empty, 'cons does not mutate:  it makes a new list');
      
      deepEqual(list([empty]), oneLi, 'the 1st arg may be of any type ...');
    });


    test("car", function() {
      var car = funcs.car,
          twoEl = list([3, 4]),
          listFirst = list([list([14])]);
      
      deepEqual(3, car.fapply([twoEl]), 'car returns the first element of a list, ');
      
      deepEqual(list([14]), car.fapply([listFirst]), 'which may be a list');
      
      expectException(function() {
          car.fapply([empty]);
      }, 'ValueError', 'taking the car of an empty list throws an exception');
    });
      

    test("cdr", function() {
      var cdr = funcs.cdr,
          fourEl = list([3, 4, 10, 'hello']),
          oneEl = list([64]);

      deepEqual(
          list([4, 10, 'hello']), 
          cdr.fapply([fourEl]),
          "cdr returns the 'rest' of a list after the first element"
      );
      
      deepEqual(empty, cdr.fapply([oneEl]), 'the cdr of a one-element list is an empty list');

      expectException(function() {
          cdr.fapply([empty]);
      }, 'ValueError', 'trying to take the cdr of an empty list throws an exception');

    });
    
    
    test("null?", function() {
        var n = funcs['null?'],
            empty = list([]);

        deepEqual(data.Boolean(true), n.fapply([empty]), "'null?' takes one argument:  a list");
        
        deepEqual(data.Boolean(false), n.fapply([list([str("list")])]), "it returns true if the list is empty, and false otherwise");
        
    });


    test("+", function() {
      var plus = funcs['+'];

      deepEqual(num(32), plus.fapply([num(27), num(5)]), "'+' is for adding two numbers");

      deepEqual(num(14), plus.fapply([num(18), num(-4)]), "they can be positive or negative");

      deepEqual(num(-17), plus.fapply([num(-9), num(-8)]), "or both negative");
    });


    test("neg", function() {
      var neg = funcs.neg,
          p3 = num(3),
          m3 = num(-3),
          m14 = num(-14);

      deepEqual(m3, neg.fapply([p3]), "'neg' negates a number, flipping the sign");

      deepEqual(m14, neg.fapply([neg.fapply([m14])]), "a number is its own double negative");
    });
    
    
    test("number-<", function() {
        var lt = funcs['number-<'];

        deepEqual(data.Boolean(false), lt.fapply([num(2), num(1)]), "'number-<' takes two numbers and compares them");
        
        deepEqual(data.Boolean(true), lt.fapply([num(11), num(39)]), "it returns true if the first is < the second");
        
        deepEqual(data.Boolean(false), lt.fapply([num(4), num(4)]), "and false otherwise -- including if they're the same");
    });


    test("eq?", function() {
      
      var eq = funcs['eq?'],
          db = Data.Boolean,
          t = db(true),
          f = db(false),
          ch1 = ch('x'),
          ch2 = ch('y');

      deepEqual(t, eq.fapply([t, t]), 'booleans');
      deepEqual(f, eq.fapply([f, t]), 'booleans');
      
      deepEqual(f, eq.fapply([num(3), num(31)]), 'numbers');
      deepEqual(t, eq.fapply([num(2331), num(2331)]), 'numbers');
      
      deepEqual(t, eq.fapply([ch1, ch1]), 'chars');
      deepEqual(f, eq.fapply([ch1, ch2]), 'chars');
      
      deepEqual(t, eq.fapply([sym('abc'), sym('abc')]), 'symbols');
      deepEqual(f, eq.fapply([sym('abc'), sym('def')]), 'symbols');
      
      expectException(function() {
          eq.fapply([num(16), db(true)]);
      }, 'TypeError', "'eq?' arguments must be of the same type");
            
      expectException(function() {
          eq.fapply([list(7), list(7)]);
      }, 'TypeError', "'eq?' does not work on lists");
      
    });
    
    
    test("datum", function() {
        var dt = funcs.datum,
            Dat = data.Datum;
        
        deepEqual(
            Dat('string', str('matt')), 
            dt.fapply([str('string'), str('matt')]), 
            "'datum' takes two arguments:  a string (list of chars) and a value"
        );
        
        expectException(function() {
            dt.fapply([num(4), str('matt')]);
        }, 'TypeError', "'datum' requires a *list* of chars as first argument");

        expectException(function() {
            dt.fapply([list([ch('a'), num(4)]), num(3)]);
        }, 'TypeError', "'datum' requires a list of *chars* as first argument");
        
        expectException(function() {
            dt.fapply([str(''), num(64)]);
        }, 'ValueError', "a *non-empty* list of chars");
        
        expectException(function() {
            dt.fapply([str('list'), num(32)]);
        }, 'ValueError', "built-in type names may not be used, because of the potential for confusion");
    });
    
    
    test("type", function() {
    	var type = funcs.type;

    	deepEqual(str("number"), type.fapply([num(3)]), "'type' takes one argument, and returns the type as a list of chars");
    	
    	deepEqual(str("blargh"), type.fapply([data.Datum('blargh', num(18))]), 'it works on built-ins as well as user-defined types');
    });
    
    
    test("value", function() {
        var val = funcs.value;
        
        deepEqual(num(3), val.fapply([data.Datum('something', num(3))]), "'value' gets the value of an object of a user-defined type");
        
        expectException(function() {
        	val.fapply([num(13)]);
        }, 'TypeError', "it doesn't work on built-ins, because the value is an implementation detail");
    });
    
    
    test("function number of arguments", function() {
        var n_args = [
                [],
                [str("abc")],
                [str("abc"), str("abc")],
                [str("abc"), str("abc"), str("abc")],
                [str("abc"), str("abc"), str("abc"), str("abc")]
            ],
            f, numArgs;
        
        for(var fname in Functions) {
            f = Functions[fname];
            numArgs = f.argTypes.length;
            expectException(function() {
                f.fapply(n_args[numArgs - 1]);
            }, 'NumArgsError', 'function ' + fname + ': too few arguments');
            expectException(function() {
                f.fapply(n_args[numArgs + 1]);
            }, 'NumArgsError', 'function ' + fname + ': too many arguments');
        }
        
    });
    
    
    function replace(array, index, newValue) {
        var newArray = array.map(function(x) {return x;});
        if(!newArray.hasOwnProperty(index) && !(index < newArray.length)) {
            throw new Error("can't set replace index " + index + ", doesn't exist");
        }
        newArray[index] = newValue;
        return newArray;
    }
    
    
    test("function argument types", function() {
        var funcs = {
                'cons'     : [num(3), list([])],
                'car'      : [list([num(4)])],
                'cdr'      : [list([num(14)])],
                'null?'    : [list([num(44)])],
                '+'        : [num(3), num(21)],
                'neg'      : [num(3)],
                'number-<' : [num(3), num(21)],
                'eq?'      : [num(3), num(21)],
                'datum'    : [str('abc'), num(8)],
                'type'     : [data.Datum('abc', num(8))],
                'value'    : [data.Datum('abc', num(8))]
            },
            types = {
                'number' : Data.Char('c'),
                'char'   : Data.Boolean(true),
                'boolean': Data.Symbol('sy'),
                'symbol' : Data.List([]),
                'list'   : Data.Number(14)
            },
            i, j, f, newArgs, fname, tempArgs;
        
        // for each function
        for(fname in funcs) {
            f = Functions[fname];
            newArgs = funcs[fname]; // should do this by reading the function's data instead
            // for each argument to that function
            for(j = 0; j < newArgs.length; j++) {
                if(f.argTypes[j] !== null) {
                    tempArgs = replace(newArgs, j, types[newArgs[j].type]);
                    expectException(function() {
                        f.fapply(tempArgs);
                    }, 'TypeError', 'for argument ' + (j + 1) + ' of function ' + fname);
                } else {
                    // if it's null, then no type error is possible,
                    // so we don't need to check it
                    ok(true, "arg " + (j + 1) + " to " + fname + " may be of any type");
                }
            }
        }
    });
    
}