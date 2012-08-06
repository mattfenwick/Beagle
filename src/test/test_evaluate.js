
function testEvaluate(evaluate, parser, data, envir, testHelper) {

    var ev = evaluate,
        expectExc = testHelper.expectException,
        num = data.Number,
        lis = data.List,
        empty = lis([]),
        str = data.makeCharList,
        anum = parser.ASTNumber,
        sym = parser.Symbol,
        alist = parser.ASTList,
        app = parser.Application,
        astr = parser.expandString;
    
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
        def(env, [sym('e'), anum('14')]);
        deepEqual(env.getBinding('e'), num(14), 'and creates a binding for that symbol');
      
        expectExc(function() {
            def(env, [sym('b'), anum('13')]);
        }, 'ValueError', "once created, bindings cannot be changed");
        equal(4, env.getBinding('b'), 'the previous value is still visible');
        
        equal(3, env.getBinding('a'), 'bindings are visible in nested scopes');

        def(env, [sym('a'), astr("derr")]);
        deepEqual(str("derr"), env.getBinding('a'), "unless they're shadowed:");
        ok(env.hasOwnBinding('a'), 'shadowing occurs when both the nested scope ...');
        ok(par.hasOwnBinding('a'), '*and* the parent scope have bindings for the same symbol');

        expectExc(function() {
            def(env, [sym('abc')]);
        }, 'NumArgsError', 'too few arguments throws an exception ...');

        expectExc(function() {
            def(env, [sym('def'), alist([]), alist([])]);
        }, 'NumArgsError', 'too many arguments is also a problem');

        expectExc(function() {
            def(env, [anum('11'), num(12)]);
        }, 'TypeError', 'the first argument must be a Beagle symbol');
    });
    
    
    test("set!", function() {
        var set = ev['set!'],
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});
        
        set(env, [sym('b'), anum(12)]);
        deepEqual(num(12), env.getBinding('b'), "set! takes two arguments, a symbol and a value");
        
        set(par, [sym('a'), anum(32)]);
        deepEqual(num(32), par.getBinding('a'), "and sets binding for the symbol to the value");
        
        set(env, [sym('a'), anum(64)]);
        deepEqual(
            [num(64), num(64)], 
            [par.getBinding('a'), env.getBinding('a')], 
            "the first binding found for that symbol will be changed"
        );
        
        expectExc(function() {
            set(env, [sym('e'), anum(88)]);
        }, 'ValueError', "set! may not be used on undefined symbols");

        expectExc(function() {
            set(env, [sym('abc')]);
        }, 'NumArgsError', 'remember that it takes two arguments');

        expectExc(function() {
            set(env, [sym('def'), empty, empty]);
        }, 'NumArgsError', '... no more, no less');

        expectExc(function() {
            set(env, [anum(11), anum(12)]);
        }, 'TypeError', 'and that the first argument must be a Beagle symbol');
    });
    
    
    test("cond", function() {
      var cond = ev['cond'],
          env = ev.getDefaultEnv(),
          ts = sym('true'),
          fs = sym('false'),
          t = Data.Boolean(true),
          f = Data.Boolean(false);
      
      env.addBinding('fsym', f);
      
      deepEqual(
          num(4),
          cond(env, [alist([ts, anum(4)]), alist([ts, astr("huh?")])]), 
          "'cond' looks through its arguments for a list whose first element evaluates to true"
      );
      
      deepEqual(
          str("huh?"), 
          cond(env, [alist([sym('fsym'), sym('fsym')]), alist([ts, astr("huh?")])]), 
          'and evaluates and returns the second element of that list'
      );

      expectExc(function() {
          cond(env, [alist([t])]);
      }, 'NumArgsError', 'lists with fewer than 2 elements are a no-no');

      expectExc(function() {
          cond(env, [alist([t, t, t])]);
      }, 'NumArgsError', 'as are lists with more than 2 elements');

      expectExc(function() {
          cond(env, [alist([fs, anum(11)])]);
      }, 'ValueError', "watch out: 'cond' is unhappy if nothing's true");

    });
    
    
    test("lambda", function() {
        var lam = ev.lambda,
            env = ev.getDefaultEnv(),
            args1 = alist([]),
            body1 = anum(4),
            args2 = alist([sym('abc')]),
            body2 = sym('abc'),
            args3 = alist([sym('q'), sym('r')]),
            body3 = app(sym('+'), [sym('q'), anum(4)]);
        
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
            lam(env, [anum('11'), body1]);
        }, 'TypeError', 'the first argument must be a list');

        expectExc(function() {
            lam(env, [alist([anum('13')]), body1])
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
          int_ = anum(31),
          str1 = astr("abcde"),
          sym1 = sym('cons'),
          l1 = app(sym('car'), [alist([anum(87)])]),
          l2 = app(sym('cons'), [anum(32), alist([])]),
          l3 = app(sym('neg'), [anum(4)]),
          l4 = alist([anum(13), astr('duh'), sym('lambda')]),
          ts = sym('true'),
          appSf = app(sym('cond'), [alist([ts, ts])]),
          ts = sym('true'),
          t = data.Boolean(true);
          
      deepEqual(num(31), ev.eval(int_, env), "AST node types: 1) numbers -> Lisp number,");
          
      deepEqual(str("abcde"), ev.eval(str1, env), "2) list of chars -> Lisp list of chars,");

      deepEqual(t, ev.eval(ts, env), "3) symbol -> lookup value in current environment,");
      
      deepEqual(lis([num(13), str('duh'), data.SpecialForm(ev['lambda'])]), ev.eval(l4, env), "4) list -> Lisp list with all elements evaluated");
      
      deepEqual(num(87), ev.eval(l1, env), "5) function application -> evaluate arguments, apply function to arguments");
      
      deepEqual(t, ev.eval(appSf, env), "6) special form application -> apply special form to *unevaluated* arguments");

      expectExc(function() {
          ev.eval(sym('blarghabag'), env);
      }, 'UndefinedVariableError', 'evaluating a symbol with no binding throws an exception');
    
      deepEqual(num(-4), ev.eval(l3, env), "in an Application, the 1st element is a function/specialform which is applied to the remaining elements");
    
      deepEqual(
          t,
          ev.eval(app(sym('null?'), [alist([])]), env),
          'for function applications, the arguments are evaluated before the function is applied'
      );

      expectExc(function() {
          ev.eval(app(sym('cons'), [sym('shouldblowup'), lis([])]), env);
      }, 'UndefinedVariableError', 'thus, passing an unbound symbol to a function throws an exception ...');

      deepEqual(
          t,
          ev.eval(app(sym('cond'), [alist([ts, ts]), alist([ts, sym('couldblowup')])]), env),
          " ... but might not do so for special forms that don't always evaluate all their arguments"
      );
    });

}