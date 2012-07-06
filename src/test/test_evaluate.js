
function testEvaluate(evaluate, funcs, data, envir, testHelper) {

    var ev = evaluate,
        expectExc = testHelper.expectException,
        sym = data.Symbol,
        num = data.Number,
        lis = data.List,
        empty = lis([]),
        str = data.String;
    
    module("Evaluate");
    
    test("default environment", function() {
        var env = ev.getDefaultEnv();
        var names = [
            'define', 'lambda', 'if', 'special', 'eval', 'true', 'false',
            'cons', 'car', 'cdr', 'list', '=', '+', 'neg'
        ];

        names.map(function(n) {
            ok(env.hasBinding(n), 'binding for ' + n);
        });

        var bindings = 0;
        for(var k in env._bindings) {
            bindings++;
        }

        equal(14, bindings, 'there are currently 14 built-in special forms and functions');
        equal(14, names.length, 'and we need to test for all of them');
    });
    
    
    test("define", function() {
        var def = ev.define,
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});

        ok(!env.hasBinding('e'), 'define takes two arguments, a symbol and a string,');
        def(env, [sym('e'), num(14)]);
        deepEqual(env.getBinding('e'), num(14), 'and creates a binding for that symbol');
      
        expectExc(function() {
            def(env, [sym('b'), num(13)]);
        }, 'ValueError', "once created, bindings cannot be changed");
        equal(4, env.getBinding('b'), 'the previous value is still visible');
        
        equal(3, env.getBinding('a'), 'bindings are visible in nested scopes');

        def(env, [sym('a'), str("derr")]);
        deepEqual(str("derr"), env.getBinding('a'), "unless they're shadowed:");
        ok(env.hasOwnBinding('a'), 'shadowing occurs when both the nested scope ...');
        ok(par.hasOwnBinding('a'), 'and the parent scope have bindings for the same symbol');

        expectExc(function() {
            def(env, [sym('abc')]);
        }, 'NumArgsError', 'too few arguments throws an exception ...');

        expectExc(function() {
            def(env, [sym('def'), empty, empty]);
        }, 'NumArgsError', 'too many arguments is also a problem');

        expectExc(function() {
            def(env, [num(11), num(12)]);
        }, 'TypeError', 'the first argument must be a Beagle symbol');
    });
    
    
    test("if", function() {
      var if_ = ev['if'],
          env = ev.getDefaultEnv(),
          t = Data.Boolean(true),
          f = Data.Boolean(false);
      
      env.addBinding('fsym', f);
      
      deepEqual(
          num(4),
          if_(env, [t, num(4), str("huh?")]), 
          "'if' evaluates its 1st argument, and if that's true, evaluates and returns its second argument"
      );
      
      deepEqual(
          str("huh?"), 
          if_(env, [sym('fsym'), num(4), str("huh?")]), 
          'otherwise, it evaluates and returns its third argument'
      );

      expectExc(function() {
          if_(env, [t]);
      }, 'NumArgsError', 'too few arguments throws an exception ...');

      expectExc(function() {
    	  if_(env, [f, empty, empty, empty]);
      }, 'NumArgsError', 'too many arguments is also a problem');

      expectExc(function() {
    	  if_(env, [num(11), num(12), num(13)]);
      }, 'TypeError', 'the first argument must be evaluate to a Beagle boolean');

      deepEqual(num(64), if_(env, [t, num(64), sym('was_not_defined')]), 
    		  "watch out: 'if' is not strict in its evaluation");
    });
    
    
    test("lambda", function() {
    	ok(false, "haven't figured out spec yet");
    });
    
    
    test("special", function() {
    	ok(false, "haven't figured out spec yet");
    });
    
    
    test("true/false", function() {
    	var env = ev.getDefaultEnv();
    	
    	deepEqual(data.Boolean(true), env.getBinding('true'), 'booleans are predefined as symbols:  both true ...');
    	deepEqual(data.Boolean(false), env.getBinding('false'), '... and false');
    });
        

    test("Beagle eval primitive", function() {
    	ok(false, "haven't figured out spec yet");
    });
    
    
    test("js evaluate", function() {
      var env = ev.getDefaultEnv();
      var int_ = num(31),
          str1 = str("abcde"),
          sym1 = sym('cons'),
          l1 = lis([
              sym('car'),
              lis([sym('list'), num(87)])
          ]),
          l2 = lis([
              sym('cons'),
              str('what?'),
              lis([sym('list')])
          ]);
      
      ok(false, "haven't figured out spec yet");
          
      deepEqual(num(31), ev.eval(int_, env));
          
      deepEqual(str("abcde"), ev.eval(str1, env));
          
      deepEqual(data.Function(funcs.cons), ev.eval(sym1, env));
    
      deepEqual(lis([]), ev.eval(lis([sym('list')]), env));
    
      deepEqual(lis([num(4)]), ev.eval(lis([sym('list'), num(4)]), env));
          
      deepEqual(
        num(87),
        ev.eval(l1, env)
      );
          
      deepEqual(lis([str('what?')]), ev.eval(l2, env));
      
    });

}