define(["app/evaluate", "app/ast", "app/data", "app/environment", "test/helper"], function(evaluate, ast, data, envir, testHelper) {
    
    module("Evaluate");

    var ev = evaluate,
        expectExc = testHelper.expectException,
        num = data.Number,
        lis = data.List,
        empty = lis([]),
        str = data.makeCharList,
        anum = ast.number,
        sym = ast.symbol,
        alist = ast.list,
        app = ast.application;
    
    
    test("default environment", function() {
        var env = ev.getDefaultEnv();
        var names = [
            'true', 'false', 'cons', 'car', 'cdr', 'eq?', '+', 
            'neg', 'null?', 'number-<', 'type'
        ];

        names.map(function(n) {
            ok(env.hasBinding(n), 'binding for ' + n);
        });

        var bindings = 0;
        for(var k in env._bindings) {
            bindings++;
        }

        equal(11, bindings, 'there are 11 built-in constants and functions');
        equal(11, names.length, 'and we need to test for all of them');
    });
    
    
    test("define", function() {
        var def = ev.define,
            d = ast.define,
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});

        ok(!env.hasBinding('e'), 'define takes two arguments, a symbol and a string,');
        def(d('e', anum(14)), env);
        deepEqual(env.getBinding('e'), num(14), 'and creates a binding for that symbol');
      
        expectExc(function() {
            def(d('b', anum(13)), env);
        }, 'ValueError', "once created, bindings cannot be changed");
        equal(4, env.getBinding('b'), 'the previous value is still visible');
        
        equal(3, env.getBinding('a'), 'bindings are visible in nested scopes');

        def(d('a', ast.char('x')), env);
        deepEqual(data.Char('x'), env.getBinding('a'), "unless they're shadowed:");
        ok(env.hasOwnBinding('a'), 'shadowing occurs when both the nested scope ...');
        ok(par.hasOwnBinding('a'), '*and* the parent scope have bindings for the same symbol');
    });
    
    
    test("set", function() {
        var set = ev['set'],
            s = ast.set,
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});
        
        set(s('b', anum(12)), env);
        deepEqual(num(12), env.getBinding('b'), "set takes two arguments, a symbol and a value");
        
        set(s('a', anum(32)), par);
        deepEqual(num(32), par.getBinding('a'), "and sets binding for the symbol to the value");
        
        set(s('a', anum(64)), env);
        deepEqual(
            [num(64), num(64)], 
            [par.getBinding('a'), env.getBinding('a')], 
            "the first binding found for that symbol will be changed"
        );
        
        expectExc(function() {
            set(s('e', anum(88)), env);
        }, 'ValueError', "set may not be used on undefined symbols");
    });
    
    
    test("cond", function() {
      var cond = ev['cond'],
          c = ast.cond,
          env = ev.getDefaultEnv(),
          ts = sym('true'),
          fs = sym('false'),
          t = Data.Boolean(true),
          f = Data.Boolean(false);
      
      env.addBinding('fsym', f);
      
      deepEqual(
          num(4), // {cond [[true 4] [true "huh?"]] 2}
          cond(c([[ts, anum(4)], 
                  [ts, anum(5)]], 
                 anum(2)), 
               env), 
          "'cond' looks through its arguments for a list whose first element evaluates to true"
      );
      
      deepEqual(
          num(5), // {cond [[false false][true 5]] "huh?"}
          cond(c([[sym('fsym'), sym('fsym')],
                  [ts, anum(5)]], 
                 anum(8)),
               env), 
          'and evaluates and returns the second element of that list'
      );
      
      deepEqual(
          num(16),  // {cond [] 16} => 16
          cond(c([], anum(16)), env),
          'an empty list of pairs is okay -- it will always evaluate to the else value'
      );
      
      expectExc(function() {
          cond(c([[anum(13), anum(8)]], 
                 anum(1)), 
               env);
      }, 'TypeError', 'the conditions *must* evaluate to a boolean');
    });
    
    
    test("lambda", function() {
        var lam = ev.lambda,
            l = ast.lambda,
            env = ev.getDefaultEnv(),
            args1 = [],
            body1 = anum(4),
            args2 = ['abc'],
            body2 = sym('abc'),
            args3 = ['q', 'r'],
            body3 = app(sym('+'), [sym('q'), anum(4)]);

        var a = lam(l(args1, [], body1), env);
        deepEqual(
            'function',
            a.type,
            'lambda takes an array of symbols and an array of body forms, and returns a function which is a lexical closure'
        );
        deepEqual(
            num(4),
            a.fapply([]),
            'the closure can be evaluated -- it expects a list of arguments'
        );
        
        deepEqual(
            str('me!!'),
            lam(l(args2, [], body2), env).fapply([str('me!!')]),
            'the closure is evaluated in an environment with the parameters bound to its arguments'
        );
        
        deepEqual(
            num(89),
            lam(l(args3, [], body3), env).fapply([num(85), str('unused')]),
            'the body can be an atom or a list'
        );
        
        expectExc(function() {
            lam(l(args1, [], body1), env).fapply([num(1)]);
        }, 'NumArgsError', 'the number of arguments to the closure must also be correct');
    });
    
    
    test("true/false", function() {
        var env = ev.getDefaultEnv();
        
        deepEqual(data.Boolean(true), env.getBinding('true'), 'booleans are predefined as symbols:  both true ...');
        deepEqual(data.Boolean(false), env.getBinding('false'), '... and false');
    });
    
    
    test("js evaluate", function() {
      var env = ev.getDefaultEnv(),
          ts = sym('true'),
          t = data.Boolean(true);

      deepEqual(num(31), ev.eval(anum(31), env), "AST node types: 1) numbers -> Lisp number,");
      deepEqual(data.Char('a'), ev.eval(ast.char('a'), env), '2) char -> char');
      deepEqual(data.List([num(4)]), ev.eval(ast.list([anum(4)]), env), '3) list -> list');
      deepEqual(t, ev.eval(sym('true'), env), "4) symbol -> bound value");      
      deepEqual(num(87), ev.eval(app(sym('car'), [ast.list([anum(87)])]), env), "5) application -> evaluate & apply");
      deepEqual(t, ev.eval(ast.cond([[ts, ts]], anum(31)), env), "6) special form -> apply but *don't* evaluate");

      expectExc(function() {
          ev.eval(sym('blarghabag'), env);
      }, 'UndefinedVariableError', 'evaluating a symbol with no binding throws an exception');
    
      deepEqual(num(-4), ev.eval(app(sym('neg'), [anum(4)]), env), "applications' 1st elements must evaluate to functions");
    
      deepEqual(
          t,
          ev.eval(app(sym('null?'), [alist([])]), env),
          'for function applications, the arguments are evaluated before the function is applied'
      );

      expectExc(function() {
          ev.eval(app(sym('cons'), [sym('shouldblowup'), alist([])]), env);
      }, 'UndefinedVariableError', 'thus, passing an unbound symbol to a function throws an exception ...');

      deepEqual(
          t,
          ev.eval(ast.cond([[ts, ts], [ts, sym('couldblowup')]], sym('xyz')), env),
          " ... but might not do so for special forms that don't always evaluate all their arguments"
      );
    });

});
