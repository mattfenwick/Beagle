
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
            'true', 'false', 'cons', 'car', 'cdr', 'eq?', '+', 
            'neg', 'null?', 'number-<', 'type', 'object', 'get',
            'entries'
        ];

        names.map(function(n) {
            ok(env.hasBinding(n), 'binding for ' + n);
        });

        var bindings = 0;
        for(var k in env._bindings) {
            bindings++;
        }

        equal(14, bindings, 'there are 14 built-in constants and functions');
        equal(14, names.length, 'and we need to test for all of them');
    });
    
    
    test("define", function() {
        var def = ev.define,
            d = parser.Define,
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});

        ok(!env.hasBinding('e'), 'define takes two arguments, a symbol and a string,');
        def(d([sym('e'), anum('14')]), env);
        deepEqual(env.getBinding('e'), num(14), 'and creates a binding for that symbol');
      
        expectExc(function() {
            def(d([sym('b'), anum('13')]), env);
        }, 'ValueError', "once created, bindings cannot be changed");
        equal(4, env.getBinding('b'), 'the previous value is still visible');
        
        equal(3, env.getBinding('a'), 'bindings are visible in nested scopes');

        def(d([sym('a'), astr("derr")]), env);
        deepEqual(str("derr"), env.getBinding('a'), "unless they're shadowed:");
        ok(env.hasOwnBinding('a'), 'shadowing occurs when both the nested scope ...');
        ok(par.hasOwnBinding('a'), '*and* the parent scope have bindings for the same symbol');
    });
    
    
    test("set", function() {
        var set = ev['set'],
            s = parser.set,
            par = envir.Environment(false, {'a': 3}),
            env = envir.Environment(par, {'b': 4});
        
        set(s([sym('b'), anum('12')]), env);
        deepEqual(num(12), env.getBinding('b'), "set takes two arguments, a symbol and a value");
        
        set(s([sym('a'), anum('32')]), par);
        deepEqual(num(32), par.getBinding('a'), "and sets binding for the symbol to the value");
        
        set(s([sym('a'), anum('64')]), env);
        deepEqual(
            [num(64), num(64)], 
            [par.getBinding('a'), env.getBinding('a')], 
            "the first binding found for that symbol will be changed"
        );
        
        expectExc(function() {
            set(s([sym('e'), anum('88')]), env);
        }, 'ValueError', "set may not be used on undefined symbols");
    });
    
    
    test("cond", function() {
      var cond = ev['cond'],
          c = parser.Cond,
          env = ev.getDefaultEnv(),
          ts = sym('true'),
          fs = sym('false'),
          t = Data.Boolean(true),
          f = Data.Boolean(false);
      
      env.addBinding('fsym', f);
      
      deepEqual(
          num(4), // {cond [[true 4] [true "huh?"]] 2}
          cond(c([alist([alist([ts, anum('4')]), 
                         alist([ts, astr("huh?")])]), 
                  anum('2')]), env), 
          "'cond' looks through its arguments for a list whose first element evaluates to true"
      );
      
      deepEqual(
          num(5), // {cond [[false false][true 5]] "huh?"}
          cond(c([alist([alist([sym('fsym'), sym('fsym')]),
                         alist([ts, anum('5')])]), 
                  astr("huh?")]), env), 
          'and evaluates and returns the second element of that list'
      );
      
      deepEqual(
          num(16),  // {cond [] 16} => 16
          cond(c([alist([]), anum('16')]), env),
          'an empty list of pairs is okay -- it will always evaluate to the else value'
      );
      
      expectExc(function() {
          cond(c([alist([alist([anum('13'), anum('8')])]), anum('1')]), env);
      }, 'TypeError', 'the conditions *must* evaluate to a boolean');
    });
    
    
    test("lambda", function() {
        var lam = ev.lambda,
            l = parser.Lambda,
            env = ev.getDefaultEnv(),
            args1 = alist([]),
            body1 = anum('4'),
            args2 = alist([sym('abc')]),
            body2 = sym('abc'),
            args3 = alist([sym('q'), sym('r')]),
            body3 = app([sym('+'), sym('q'), anum('4')]);
        
        var a = lam(l([args1, body1]), env);
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
            lam(l([args2, body2]), env).fapply([str('me!!')]),
            'the closure is evaluated in an environment with the parameters bound to its arguments'
        );
        
        deepEqual(
            num(89),
            lam(l([args3, body3]), env).fapply([num(85), str('unused')]),
            'the body can be an atom or a list'
        );
        
        expectExc(function() {
            lam(l([args1, body1]), env).fapply([num(1)]);
        }, 'NumArgsError', 'the number of arguments to the closure must also be correct');
    });
    
    
    test("true/false", function() {
        var env = ev.getDefaultEnv();
        
        deepEqual(data.Boolean(true), env.getBinding('true'), 'booleans are predefined as symbols:  both true ...');
        deepEqual(data.Boolean(false), env.getBinding('false'), '... and false');
    });
    
    
    test("js evaluate", function() {
      var env = ev.getDefaultEnv(),
          int_ = anum('31'),
          str1 = astr("abcde"),
          sym1 = sym('cons'),
          l1 = app([sym('car'),  alist([anum('87')])]),
          l2 = app([sym('cons'), anum('32'), alist([])]),
          l3 = app([sym('neg'),  anum('4')]),
          l4 = alist([anum('13'), astr('duh'), sym('true')]),
          ts = sym('true'),
          appSf = parser.Cond([alist([alist([ts, ts])]), int_]),
          ts = sym('true'),
          t = data.Boolean(true);
          
      deepEqual(num(31), ev.eval(int_, env), "AST node types: 1) numbers -> Lisp number,");
          
      deepEqual(str("abcde"), ev.eval(str1, env), "2) list of chars -> Lisp list of chars,");

      deepEqual(t, ev.eval(ts, env), "3) symbol -> lookup value in current environment,");
      
      deepEqual(lis([num(13), str('duh'), data.Boolean(true)]), ev.eval(l4, env), "4) list -> Lisp list with all elements evaluated");
      
      deepEqual(num(87), ev.eval(l1, env), "5) function application -> evaluate arguments, apply function to arguments");
      
      deepEqual(t, ev.eval(appSf, env), "6) special form application -> apply special form to *unevaluated* arguments");

      expectExc(function() {
          ev.eval(sym('blarghabag'), env);
      }, 'UndefinedVariableError', 'evaluating a symbol with no binding throws an exception');
    
      deepEqual(num(-4), ev.eval(l3, env), "in an Application, the 1st element is a function/specialform which is applied to the remaining elements");
    
      deepEqual(
          t,
          ev.eval(app([sym('null?'), alist([])]), env),
          'for function applications, the arguments are evaluated before the function is applied'
      );

      expectExc(function() {
          ev.eval(app([sym('cons'), sym('shouldblowup'), alist([])]), env);
      }, 'UndefinedVariableError', 'thus, passing an unbound symbol to a function throws an exception ...');

      deepEqual(
          t,
          ev.eval(parser.Cond([alist([alist([ts, ts]), alist([ts, sym('couldblowup')])]), sym('xyz')]), env),
          " ... but might not do so for special forms that don't always evaluate all their arguments"
      );
    });

}