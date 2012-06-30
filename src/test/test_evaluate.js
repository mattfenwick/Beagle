
function testEvaluate(evaluate, funcs, data, Environment) {

    var ev = evaluate;
    
    module("Evaluate");
    
    test("default env", function() {
      var env = Evaluate.getDefaultEnv();
      var names = ['cons', 'car', 'cdr', 'list', 'define', 'lambda', 'if', '='];
      names.map(function(n) {ok(env.hasBinding(n), 'binding ' + n);});
    });
    
    
    test("define", function() {
      var def = Evaluate.define,
          par = Environment.Environment(false, {'a': 3}),
          env = Environment.Environment(par, {'b': 4}),
          raised = false;
      
      try {
        def(env, Data.Symbol('b'), Data.Number(13));
      } catch(e) {
        raised = true;
      };
      ok(raised, "can't change binding");
      equal(4, env.getBinding('b'));
      
      def(env, Data.Symbol('c'), Data.String("derr"));
      deepEqual(Data.String("derr"), env.getBinding('c'));
      ok(env.hasOwnBinding('c'));
      ok(!par.hasBinding('c'));
    });
    
    
    test("if", function() {
      var i = evaluate.i;
      var env = evaluate.getDefaultEnv();
      
      deepEqual(Data.String("huh?"), evaluate.eval(Data.List([Data.Symbol('if'), Data.Boolean(false),
          Data.Number(4), Data.String("huh?")]), env));
    });
    
    
    test("evaluate", function() {
      var env = evaluate.getDefaultEnv();
      var int_ = data.Number(31),
          str = data.String("abcde"),
          sym = data.Symbol('cons'),
          l1 = data.List([
              data.Symbol('car'),
              data.List([data.Symbol('list'), data.Number(87)])
          ]),
          l2 = data.List([
              data.Symbol('cons'),
              data.String('what?'),
              data.List([data.Symbol('list')])
          ])
          ;
          
      deepEqual(data.Number(31), evaluate.eval(int_, env));
          
      deepEqual(data.String("abcde"), evaluate.eval(str, env));
          
      deepEqual(data.Function(funcs.cons), evaluate.eval(sym, env));
    
      deepEqual(data.List([]), ev.eval(data.List([data.Symbol('list')]), env));
    
      deepEqual(data.List([data.Number(4)]), ev.eval(data.List([data.Symbol('list'), data.Number(4)]), env));
          
      deepEqual(
        data.Number(87),
        evaluate.eval(l1, env)
      );
          
      deepEqual(data.List([data.String('what?')]), evaluate.eval(l2, env));
      
    });

}