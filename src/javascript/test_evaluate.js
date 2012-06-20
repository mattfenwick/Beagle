
function testEvaluate(evaluate, funcs, data) {

    var ev = evaluate;

    module("primitives");

    test("makePrimitives", function() {
      expect(12);

      function sym(v) {
        return {'type': 'symbol', 'value': v};
      }

      var int_ = sym("345"),
          empty = sym(""),
          float1 = sym("03."),
          float2 = sym("3.456"),
          float3 = sym(".001"),
          float4 = sym("0.01"),
          str = {'type': 'string', 'value': "yes this is a string"},
          notstr = sym('"open'),
          sym1 = sym('*?#""/'),
          list1 = {'type': 'list', 'value': [sym("+"), {'type': 'string', 'value': 'str1'}, sym("345")]},
          list2 = {
            'type': 'list',
            'value': [
              sym("+"), 
              {'type': 'list', 'value': [sym("-"), sym("34.32")]}, 
              sym('"omg')
            ]
          };
          b1 = sym("true");
          
      deepEqual(data.Number(345), ev.makePrimitives(int_));
          
      try {
        var e = ev.makePrimitives(empty);
        ok(0);
      } catch(e) {
        ok(1, "can't make primitive of empty string");
      };
          
      deepEqual(data.Number(3), ev.makePrimitives(float1), "float with decimal point");
          
      deepEqual(data.Number(3.456), evaluate.makePrimitives(float2), "float with leading and trailing digits");
          
      deepEqual(data.Number(0.001), evaluate.makePrimitives(float3), "float with leading decimal point");
          
      deepEqual(data.Number(0.01), evaluate.makePrimitives(float4), "float with leading 0");
          
      deepEqual(data.String("yes this is a string"), ev.makePrimitives(str), 'string');
          
      deepEqual(data.Symbol('"open'), evaluate.makePrimitives(notstr), 'symbol (with leading ")');
          
      deepEqual(data.Symbol('*?#""/'), evaluate.makePrimitives(sym1), 'symbol with funky chars');
          
      var l1 = data.List([data.Symbol('+'), data.String('str1'), data.Number(345)]);
      deepEqual(l1, ev.makePrimitives(list1), 'simple list');
          
      var l2 = data.List([
          data.Symbol('+'),
          data.List([data.Symbol('-'), data.Number(34.32)]),
          data.Symbol('"omg')
      ]);
      deepEqual(l2, evaluate.makePrimitives(list2));
      
      deepEqual(data.Symbol('true'), evaluate.makePrimitives(b1));
    });
    
    
    test("default env", function() {
      var env = Evaluate.getDefaultEnv();
      var names = ['cons', 'car', 'cdr', 'list', 'define', 'lambda', 'if', '='];
      names.map(function(n) {ok(env.hasBinding(n), 'binding ' + n);});
    });
    
    
    test("define", function() {
      var def = Evaluate.define,
          par = Evaluate.Environment(false, {'a': 3}),
          env = Evaluate.Environment(par, {'b': 4}),
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
    
    
    test("environment lookup", function() {
      var par = Evaluate.Environment(false, {'a': 3}),
          env = Evaluate.Environment(par, {'b': 4}),
          raised = false;
          
      // can find if in top env
      ok(env.hasBinding('b'));
      ok(env.hasOwnBinding('b'));
      equal(4, env.getBinding('b'));
      
      // can find if in parent env
      ok(env.hasBinding('a'));
      ok(!env.hasOwnBinding('a'));
      equal(3, env.getBinding('a'));
      
      // can't find if in none env
      ok(!env.hasBinding('c'));
      ok(!env.hasOwnBinding('c'));
      try {
        equal(false, env.getBinding('c'));
      } catch(e) {
        raised = true;
      };
      ok(raised, "can not get value");
      
      // can add new binding
      raised = false;
      try {
        env.addBinding('e', 13);
      } catch(e) {
        raised = true;
      };
      ok(!raised, "can add new binding");
      
      // can't add binding if already there
      raised = false;
      try {
        env.addBinding('b', 45);
      } catch(e) {
        raised = true;
      };
      ok(raised, "can't add binding if already there");
      equal(4, env.getBinding('b'));
      
      // but can shadow parent binding
      raised = false;
      try {
        env.addBinding('a', 32);
      } catch(e) {
        raised = true;
      };
      ok(!raised, "ok to shadow parent binding");
      equal(32, env.getBinding('a'), "binding shadowed");
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