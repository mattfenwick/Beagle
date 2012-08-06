
function testEvaluate(evaluate, parser, data, envir, testHelper) {

    var ev = evaluate,
        expectExc = testHelper.expectException,
        num = data.Number,
        lis = data.List,
        empty = lis([]),
        str = data.makeCharList,
        astn = parser.ASTNode;
    
    module("Evaluate");
    
    
    test("default environment", function() {
        var env = ev.getDefaultEnv();
        var names = [
            'define', 'lambda', 'true', 'false',
            'cons', 'car', 'cdr', 'eq?', '+', 'neg', 'set!',
            'cond', 'null?', 'number-<', 'datum', 'type', 'value'
        ];

        names.map(function(n) {
            ok(env.hasBinding(n), 'binding for ' + n);
        });

        var bindings = 0;
        for(var k in env._bindings) {
            bindings++;
        }

        equal(17, bindings, 'there are 17 built-in special forms, constants, and functions');
        equal(17, names.length, 'and we need to test for all of them');
    });
    
    
    test("define", function() {
        var def = ev.define,
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});

        ok(!env.hasBinding('e'), 'define takes two arguments, a symbol and a string,');
        def(env, [astn('symbol', 'e'), astn('number', 14)]);
        deepEqual(env.getBinding('e'), num(14), 'and creates a binding for that symbol');
      
        expectExc(function() {
            def(env, [astn('symbol', 'b'), astn('number', 13)]);
        }, 'ValueError', "once created, bindings cannot be changed");
        equal(4, env.getBinding('b'), 'the previous value is still visible');
        
        equal(3, env.getBinding('a'), 'bindings are visible in nested scopes');

        def(env, [astn('symbol', 'a'), astn('list', parser.expandString("derr"))]);
        deepEqual(str("derr"), env.getBinding('a'), "unless they're shadowed:");
        ok(env.hasOwnBinding('a'), 'shadowing occurs when both the nested scope ...');
        ok(par.hasOwnBinding('a'), '*and* the parent scope have bindings for the same symbol');

        expectExc(function() {
            def(env, [astn('symbol', 'abc')]);
        }, 'NumArgsError', 'too few arguments throws an exception ...');

        expectExc(function() {
            def(env, [astn('symbol', 'def'), empty, empty]);
        }, 'NumArgsError', 'too many arguments is also a problem');

        expectExc(function() {
            def(env, [astn('number', 11), num(12)]);
        }, 'TypeError', 'the first argument must be a Beagle symbol');
    });
    
    
    test("set!", function() {
        var set = ev['set!'],
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});
        
        set(env, [astn('symbol', 'b'), astn('number', 12)]);
        deepEqual(num(12), env.getBinding('b'), "set! takes two arguments, a symbol and a value");
        
        set(par, [astn('symbol', 'a'), astn('number', 32)]);
        deepEqual(num(32), par.getBinding('a'), "and sets binding for the symbol to the value");
        
        set(env, [astn('symbol', 'a'), astn('number', 64)]);
        deepEqual(
            [num(64), num(64)], 
            [par.getBinding('a'), env.getBinding('a')], 
            "the first binding found for that symbol will be changed"
        );
        
        expectExc(function() {
            set(env, [astn('symbol', 'e'), astn('number', 88)]);
        }, 'ValueError', "set! may not be used on undefined symbols");

        expectExc(function() {
            set(env, [astn('symbol', 'abc')]);
        }, 'NumArgsError', 'remember that it takes two arguments');

        expectExc(function() {
            set(env, [astn('symbol', 'def'), empty, empty]);
        }, 'NumArgsError', '... no more, no less');

        expectExc(function() {
            set(env, [astn('number', 11), astn('number', 12)]);
        }, 'TypeError', 'and that the first argument must be a Beagle symbol');
    });
    
    
    test("cond", function() {
      var cond = ev['cond'],
          env = ev.getDefaultEnv(),
          ts = astn('symbol', 'true'),
          fs = astn('symbol', 'false'),
          t = Data.Boolean(true),
          f = Data.Boolean(false);
      
      env.addBinding('fsym', f);
      
      deepEqual(
          num(4),
          cond(env, [astn('list', [ts, astn('number', 4)]), astn('list', [ts, str("huh?")])]), 
          "'cond' looks through its arguments for a list whose first element evaluates to true"
      );
      
      deepEqual(
          str("huh?"), 
          cond(env, [astn('list', [astn('symbol', 'fsym'), astn('symbol', 'fsym')]), astn('list', [ts, astn('list', parser.expandString("huh?"))])]), 
          'and evaluates and returns the second element of that list'
      );

      expectExc(function() {
          cond(env, [astn('list', [t])]);
      }, 'NumArgsError', 'lists with fewer than 2 elements are a no-no');

      expectExc(function() {
          cond(env, [astn('list', [t, t, t])]);
      }, 'NumArgsError', 'as are lists with more than 2 elements');

      expectExc(function() {
          cond(env, [astn('list', [fs, astn('number', 11)])]);
      }, 'ValueError', "watch out: 'cond' is unhappy if nothing's true");

    });
    
    
    test("lambda", function() {
        var lam = ev.lambda,
            env = ev.getDefaultEnv(),
            args1 = astn('list', []),
            body1 = astn('number', 4),
            args2 = astn('list', [astn('symbol', 'abc')]),
            body2 = astn('symbol', 'abc'),
            args3 = astn('list', [astn('symbol', 'q'), astn('symbol', 'r')]),
            body3 = astn('application', {'operator': astn('symbol', '+'), 'arguments': [astn('symbol', 'q'), astn('number', 4)]});
        
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
            lam(env, [astn('number', '11'), body1]);
        }, 'TypeError', 'the first argument must be a list');

        expectExc(function() {
            lam(env, [astn('list', [astn('number', '13')]), body1])
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
    
    
    test("js evaluate", function() {
      var env = ev.getDefaultEnv(),
          int_ = astn('number', 31),
          str1 = astn('list', parser.expandString("abcde")),
          sym1 = astn('symbol', 'cons'),
          l1 = astn('application', 
              {'operator': astn('symbol', 'car'),
               'arguments': [astn('list', [astn('number', 87)])]}),
          l2 = astn('application',
              {'operator': astn('symbol', 'cons'), 'argumments': [astn('number', 32), astn('list', [])]}),
          l3 = astn('application', {'operator': astn('symbol', 'neg'), 'arguments': [astn('number', 4)]}),
          l4 = astn('list', [astn('number', 13), astn('list', parser.expandString('duh')), astn('symbol', 'lambda')]),
          ts = astn('symbol', 'true'),
          appSf = astn('application', {'operator': astn('symbol', 'cond'), 'arguments': [astn('list', [ts, ts])]}),
          ts = astn('symbol', 'true'),
          t = data.Boolean(true);
          
      deepEqual(num(31), ev.eval(int_, env), "AST node types: 1) numbers -> Lisp number,");
          
      deepEqual(str("abcde"), ev.eval(str1, env), "2) list of chars -> Lisp list of chars,");

      deepEqual(t, ev.eval(ts, env), "3) symbol -> lookup value in current environment,");
      
      deepEqual(lis([num(13), str('duh'), data.SpecialForm(ev['lambda'])]), ev.eval(l4, env), "4) list -> Lisp list with all elements evaluated");
      
      deepEqual(num(87), ev.eval(l1, env), "5) function application -> evaluate arguments, apply function to arguments");
      
      deepEqual(t, ev.eval(appSf, env), "6) special form application -> apply special form to *unevaluated* arguments");

      expectExc(function() {
          ev.eval(astn('symbol', 'blarghabag'), env);
      }, 'UndefinedVariableError', 'evaluating a symbol with no binding throws an exception');
    
      deepEqual(num(-4), ev.eval(l3, env), "in an Application, the 1st element is a function/specialform which is applied to the remaining elements");

      expectExc(function() {
          ev.eval(astn('application', {'operator': false, 'arguments': []}), env);
      }, 'ValueError', 'trying to create an empty Application throws an exception');
    
      deepEqual(
          t,
          ev.eval(astn('application', {'operator': astn('symbol', 'null?'), 'arguments': [astn('list', [])]}), env),
          'for function applications, the arguments are evaluated before the function is applied'
      );

      expectExc(function() {
          ev.eval(astn('application', {'operator': astn('symbol', 'cons'), 'arguments': [astn('symbol', 'shouldblowup'), lis([])]}), env);
      }, 'UndefinedVariableError', 'thus, passing an unbound symbol to a function throws an exception ...');

      deepEqual(
          t,
          ev.eval(astn('application', {'operator': astn('symbol', 'cond'), 
                      'arguments': [astn('list', [ts, ts]), astn('list', [ts, astn('symbol', 'couldblowup')])]}), 
                  env),
          " ... but might not do so for special forms that don't always evaluate all their arguments"
      );
    });

}