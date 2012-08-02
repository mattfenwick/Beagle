
function testEvaluate(evaluate, funcs, data, envir, testHelper) {

    var ev = evaluate,
        expectExc = testHelper.expectException,
        sym = data.Symbol,
        num = data.Number,
        lis = data.List,
        empty = lis([]),
        str = data.makeCharList,
        app = data.Application;
    
    module("Evaluate");
    
    
    test("default environment", function() {
        var env = ev.getDefaultEnv();
        var names = [
            'define', 'lambda', 'quote', 'eval', 'true', 'false',
            'cons', 'car', 'cdr', 'eq?', '+', 'neg', 'set!',
            'cond', 'null?', 'number-<'
        ];

        names.map(function(n) {
            ok(env.hasBinding(n), 'binding for ' + n);
        });

        var bindings = 0;
        for(var k in env._bindings) {
            bindings++;
        }

        equal(16, bindings, 'there are 16 built-in special forms and functions');
        equal(16, names.length, 'and we need to test for all of them');
    });


    test("quote", function() {
        var q = ev.quote,
            env = ev.getDefaultEnv();

        deepEqual(sym('ghi'), q(env, [sym('ghi')]), "'quote' takes one argument and returns it unevaluated");

        expectExc(function() {
            q(env, []);
        }, 'NumArgsError', 'too few args: error');

        expectExc(function() {
            q(env, [sym('def'), num(32)]);
        }, 'NumArgsError', 'too many: error');

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
    
    
    test("set!", function() {
    	var set = ev['set!'],
    	    par = envir.Environment(false, {'a': 3}),
    	    env = envir.Environment(par, {'b': 4});
    	
    	set(env, [sym('b'), num(12)]);
    	deepEqual(num(12), env.getBinding('b'), "set! takes two arguments, a symbol and a value");
    	
    	set(par, [sym('a'), num(32)]);
    	deepEqual(num(32), par.getBinding('a'), "and sets binding for the symbol to the value");
    	
    	set(env, [sym('a'), num(64)]);
    	deepEqual(
    	    [num(64), num(64)], 
    	    [par.getBinding('a'), env.getBinding('a')], 
    	    "the first binding found for that symbol will be changed"
    	);
    	
    	expectExc(function() {
    		set(env, [sym('e'), num(88)]);
    	}, 'ValueError', "set! may not be used on undefined symbols");

        expectExc(function() {
            set(env, [sym('abc')]);
        }, 'NumArgsError', 'remember that it takes two arguments');

        expectExc(function() {
            set(env, [sym('def'), empty, empty]);
        }, 'NumArgsError', '... no more, no less');

        expectExc(function() {
            set(env, [num(11), num(12)]);
        }, 'TypeError', 'and that the first argument must be a Beagle symbol');
    });
    
    
    test("cond", function() {
      var cond = ev['cond'],
          env = ev.getDefaultEnv(),
          t = Data.Boolean(true),
          f = Data.Boolean(false);
      
      env.addBinding('fsym', f);
      
      deepEqual(
          num(4),
          cond(env, [lis([t, num(4)]), lis([t, str("huh?")])]), 
          "'cond' looks through its arguments for a list whose first element evaluates to true"
      );
      
      deepEqual(
          str("huh?"), 
          cond(env, [lis([sym('fsym'), sym('fsym')]), lis([t, str("huh?")])]), 
          'and evaluates and returns the second element of that list'
      );

      expectExc(function() {
          cond(env, [lis([t])]);
      }, 'NumArgsError', 'lists with fewer than 2 elements are a no-no');

      expectExc(function() {
          cond(env, [lis([t, t, t])]);
      }, 'NumArgsError', 'as are lists with more than 2 elements');

      expectExc(function() {
          cond(env, [lis([f, num(11)])]);
      }, 'ValueError', "watch out: 'cond' is unhappy if nothing's true");

    });
    
    
    test("lambda", function() {
        var lam = ev.lambda,
            env = ev.getDefaultEnv(),
            args1 = lis([]),
            body1 = num(4),
            args2 = lis([sym('abc')]),
            body2 = sym('abc'),
            args3 = lis([sym('q'), sym('r')]),
            body3 = app(sym('+'), [sym('q'), num(4)]);
        
        var a = lam(env, [args1, body1]);
        deepEqual(
            'function',
            a.type,
            'lambda takes a list of symbols and a body, and returns a closure'
        );
        deepEqual(
        	num(4),
        	a.fapply([]),
        	'the closure can be evaluated -- it expects a list of arguments'
        );
        
        deepEqual(
        	str('me!!'),
        	lam(env, [args2, body2]).fapply([str('me!!')]),
        	'the closure is evaluated in an environment with the parameters bound to its arguments'
        );
        
        deepEqual(
        	num(89),
        	lam(env, [args3, body3]).fapply([num(85), str('unused')]),
        	'the body can be an atom or a list'
        );

        expectExc(function() {
            lam(env, [args1]);
        }, 'NumArgsError', 'too few args: exception');

        ok(lam(env, [args1, body1, body1]), 
            'but it may have multiple body forms (but at least 1)');

        expectExc(function() {
            lam(env, [num(11), body1]);
        }, 'TypeError', 'the first argument must be evaluate to a Beagle list');

        expectExc(function() {
            lam(env, [lis([num(13)]), body1])
        }, 'TypeError', '... and every element in that list must be a symbol');
        
        expectExc(function() {
        	lam(env, [args1, body1]).fapply([num(1)]);
        }, 'NumArgsError', 'the number of arguments to the closure must also be correct');
    
    });
    
    
    test("true/false", function() {
        var env = ev.getDefaultEnv();
        
        deepEqual(data.Boolean(true), env.getBinding('true'), 'booleans are predefined as symbols:  both true ...');
        deepEqual(data.Boolean(false), env.getBinding('false'), '... and false');
    });
        

    test("Beagle eval primitive", function() {
        var bev = ev.beagleEval,
            env = ev.getDefaultEnv();

        ok(false, "this should be a function, not a special form");

        expectExc(function() {
            bev(env, []);
        }, 'NumArgsError', 'eval expects exactly 1 argument: ');

        expectExc(function() {
            bev(env, [num(3), num(2)]);
        }, 'NumArgsError', 'no more, no less');
    });
    
    
    test("js evaluate", function() {
      var env = ev.getDefaultEnv(),
          int_ = num(31),
          str1 = str("abcde"),
          sym1 = sym('cons'),
          l1 = app(
              sym('car'),
              [lis([num(87)])]
          ),
          l2 = app(
              sym('cons'),
              [str('what?'), lis([])]
          ),
          l3 = app(sym('neg'), [num(4)]),
          l4 = lis([num(13), str('duh')]),
          t = data.Boolean(true),
          cons = funcs.cons,
          myif = data.SpecialForm(ev['if']);
          
      deepEqual(num(31), ev.eval(int_, env), "there are several self-evaluating forms: 1) numbers ...");
          
      deepEqual(str("abcde"), ev.eval(str1, env), "... 2) strings ...");

      deepEqual(t, ev.eval(t, env), "... 3) booleans ...");

      deepEqual(cons, ev.eval(cons, env), "... 4) functions ...");

      deepEqual(myif, ev.eval(myif, env), "... 5) special forms");
          
      deepEqual(l4, ev.eval(l4, env), "... 6) lists");
      
      deepEqual(cons, ev.eval(sym1, env), "symbols evaluate to the current binding");

      expectExc(function() {
          ev.eval(sym('blarghabag'), env);
      }, 'UndefinedVariableError', 'evaluating a symbol with no binding throws an exception');
    
      deepEqual(num(-4), ev.eval(l3, env), "in an Application, the 1st element is a function/specialform which is applied to the remaining elements");

      expectExc(function() {
          ev.eval(app(), env);
      }, 'ValueError', 'trying to create an empty Application throws an exception');
    
      deepEqual(
          t,
          ev.eval(app(sym('null?'), [lis([])]), env),
          'for function applications, the arguments are evaluated before the function is applied'
      );

      expectExc(function() {
          ev.eval(app(sym('cons'), [sym('shouldblowup'), lis([])]), env);
      }, 'UndefinedVariableError', 'thus, passing an unbound symbol to a function throws an exception ...');

      deepEqual(
          t,
          ev.eval(app(sym('cond'), 
        		      [lis([t, t]), 
        		       lis([t, sym('shouldblowup')])]), 
        		  env),
          " ... but might not do so for special forms that don't always evaluate all their arguments"
      );
          
      deepEqual(
          num(87),
          ev.eval(l1, env),
          'this is just another example of evaluating an application'
      );
          
      deepEqual(
          lis([str('what?')]),
          ev.eval(l2, env),
          'and another list example'
      );
      
    });

}