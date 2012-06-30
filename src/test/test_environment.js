
function testEnvironment(Environment) {    
    
    test("environment lookup", function() {
      var par = Environment.Environment(false, {'a': 3}),
          env = Environment.Environment(par, {'b': 4}),
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

};