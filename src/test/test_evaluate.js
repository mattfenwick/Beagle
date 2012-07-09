
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
        var lam = ev.lambda,
            env = ev.getDefaultEnv(),
            args1 = lis([]),
            args2 = lis([sym('abc')]),
            args3 = lis([sym('q'), sym('r')]),
            body1 = num(4),
            body2 = sym('abc'),
            body3 = lis([sym('+'), sym('q'), num(4)]);
        
        var a = lam(env, [args1, body1]);
        deepEqual(
            'function',
            a.type,
            'lambda takes a list of symbols and a body, and returns a closure'
        );
        deepEqual(
        	num(4),
        	a.value([]),
        	'the closure can be evaluated -- it expects a list of arguments'
        );
        
        deepEqual(
        	str('me!!'),
        	lam(env, [args2, body2]).value([str('me!!')]),
        	'the closure is evaluated in an environment with the parameters bound to its arguments'
        );
        
        deepEqual(
        	num(89),
        	lam(env, [args3, body3]).value([num(85), str('unused')]),
        	'the body can be an atom or a list'
        );

        expectExc(function() {
            lam(env, [args1]);
        }, 'NumArgsError', 'too few arguments throws an exception ...');

        expectExc(function() {
            lam(env, [args1, body1, body1]);
        }, 'NumArgsError', 'too many arguments is also a problem');

        expectExc(function() {
            lam(env, [num(11), body1]);
        }, 'TypeError', 'the first argument must be evaluate to a Beagle list');

        expectExc(function() {
            lam(env, [lis([num(13)]), body1])
        }, 'TypeError', '... and every element in that list must be a symbol');
        
        expectExc(function() {
        	lam(env, [args1, body1]).value([num(1)]);
        }, 'NumArgsError', 'the number of arguments to the closure must also be correct');
    
    });
    
    
    test("special", function() {
        var spec = ev.special,
            env = ev.getDefaultEnv(),
            args1 = lis([]),
            args2 = lis([sym('abc')]),
            args3 = lis([sym('q'), sym('r')]),
            body1 = num(4),
            body2 = sym('abc'),
            body3 = lis([sym('+'), sym('q'), num(4)]);
        
        var a = spec(env, [args1, body1]);
        deepEqual(
            'specialform',
            a.type,
            'special takes a list of symbols and a body, and returns a closure'
        );
        deepEqual(
        	num(4),
        	a.value(env, []),
        	'the closure can be evaluated -- it expects a list of arguments'
        );
        
        deepEqual(
        	str('me!!'),
        	spec(env, [args2, body2]).value(env, [str('me!!')]),
        	'the closure is evaluated in an environment with the parameters bound to its arguments'
        );
        
        deepEqual(
        	num(89),
        	spec(env, [args3, body3]).value(env, [num(85), str('unused')]),
        	'the body can be an atom or a list'
        );

        expectExc(function() {
            spec(env, [args1]);
        }, 'NumArgsError', 'too few arguments throws an exception ...');

        expectExc(function() {
            spec(env, [args1, body1, body1]);
        }, 'NumArgsError', 'too many arguments is also a problem');

        expectExc(function() {
            spec(env, [num(11), body1]);
        }, 'TypeError', 'the first argument must be evaluate to a Beagle list');

        expectExc(function() {
            spec(env, [lis([num(13)]), body1])
        }, 'TypeError', '... and every element in that list must be a symbol');
        
        expectExc(function() {
        	spec(env, [args1, body1]).value(env, [num(143341)]);
        }, 'NumArgsError', 'the number of arguments to the closure must also be correct');
    });
    
    
    test("true/false", function() {
        var env = ev.getDefaultEnv();
        
        deepEqual(data.Boolean(true), env.getBinding('true'), 'booleans are predefined as symbols:  both true ...');
        deepEqual(data.Boolean(false), env.getBinding('false'), '... and false');
    });
        

    test("Beagle eval primitive", function() {
        ok(false, "haven't figured out spec yet -- need to change this into a function (??)");
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
          ]),
          l3 = lis([sym('list')]),
          t = data.Boolean(true),
          cons = data.Function(funcs.cons),
          myif = data.SpecialForm(ev['if']);
          
      deepEqual(num(31), ev.eval(int_, env), "there are several self-evaluating forms: 1) numbers ...");
          
      deepEqual(str("abcde"), ev.eval(str1, env), "... 2) strings ...");

      deepEqual(t, ev.eval(t, env), "... 3) booleans ...");

      deepEqual(cons, ev.eval(cons, env), "... 4) functions ...");

      deepEqual(myif, ev.eval(myif, env), "... and 5) special forms");
          
      deepEqual(cons, ev.eval(sym1, env), "symbols evaluate to the current binding");

      expectExc(function() {
          ev.eval(sym('blarghabag'), env);
      }, 'UndefinedVariableError', 'evaluating a symbol with no binding throws an exception');
    
      deepEqual(lis([]), ev.eval(l3, env), "lists: the 1st element is eval'ed to a function/specialform and applied to the rest of the list");

      expectExc(function() {
          ev.eval(lis([]), env);
      }, 'ValueError', 'trying to evaluate the empty list throws an exception');
    
      deepEqual(
          lis([cons]),
          ev.eval(lis([sym('list'), sym('cons')]), env),
          'for function applications, the arguments are evaluated before the function is applied'
      );

      expectExc(function() {
          ev.eval(lis([sym('list'), sym('shouldblowup')]), env);
      }, 'UndefinedVariableError', 'thus, passing an unbound symbol to a function throws an exception ...');

      deepEqual(
          t,
          ev.eval(lis([sym('if'), t, t, sym('shouldblowup')]), env),
          " ... but doesn't do so for special forms that don't evaluate all their arguments"
      );
          
      deepEqual(
          num(87),
          ev.eval(l1, env),
          'this is just another example of evaluating a list'
      );
          
      deepEqual(
          lis([str('what?')]),
          ev.eval(l2, env),
          'and another list example'
      );
      
    });

}