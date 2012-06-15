
function testFunctions(funcs, data) {

    if( !funcs || !data ) {
      throw new Error("can't run tests without dependencies");
    }
    

    module("functions");


    test("cons", function() {
      deepEqual(data.List([14]), funcs.cons(14, data.List([])));
      
      deepEqual(data.List([1, 2, 3]), funcs.cons(1, data.List([2, 3])));
    });


    test("car", function() {
    
      var car = funcs.car;
      deepEqual(3, car(data.List([3, 4])));
    
      // uh-oh, empty list!
      deepEqual(data.Nil(), car(data.List([])));
      
    });
      

    test("cdr", function() {
    
      var cdr = funcs.cdr;
      deepEqual(
          data.List([4, 10, 'hello']), 
          cdr(data.List([3, 4, 10, 'hello']))
      );
    
      // uh-oh !!
      deepEqual(data.Nil(), cdr(data.List([])));

    });

    test("list", function() {
      
      var list = funcs.list;
      deepEqual(data.List([3, 4, 5]), list(3, 4, 5));
      
    });

    test("equals", function() {
      
      var eq = funcs['='];
      deepEqual(Data.Boolean(true), eq(Data.Boolean(true), Data.Boolean(true)), 'booleans');
      deepEqual(Data.Boolean(false), eq(Data.Boolean(false), Data.Boolean(true)), 'booleans');
      
      deepEqual(Data.Boolean(true), eq(Data.Number(31), Data.Number(31)), 'number');
      deepEqual(Data.Boolean(false), eq(Data.Number(3), Data.Number(31)), 'number');
      deepEqual(Data.Boolean(true), eq(Data.Number(2331), Data.Number(2331)), 'number');
      
      deepEqual(Data.Boolean(true), eq(Data.String("xyz"), Data.String("xyz")), 'strings');
      deepEqual(Data.Boolean(false), eq(Data.String("yz"), Data.String("xyz")), 'strings');
      
      deepEqual(Data.Boolean(true), eq(Data.Symbol('abc'), Data.Symbol('abc')), 'symbols');
      deepEqual(Data.Boolean(false), eq(Data.Symbol('abc'), Data.Symbol('def')), 'symbols');
      
      deepEqual(Data.Nil(), eq(Data.Symbol('abc'), Data.String('abc')), 'mixed types');
      deepEqual(Data.Nil(), eq(Data.Number(16), Data.Boolean(true)), 'mixed types');
      
      deepEqual(Data.Boolean(true), eq(Data.List([]), Data.List([])), 'empty lists');
      deepEqual(Data.Boolean(false), eq(Data.List([]), Data.List([Data.Number(3)])), 'empty and non-empty lists');
      deepEqual(Data.Boolean(true), eq(Data.List([Data.Number(3)]), Data.List([Data.Number(3)])), '2 non-empty lists');
      deepEqual(Data.Boolean(false), eq(Data.List([Data.Number(3)]), Data.List([Data.String('3')])), 'empty and non-empty lists');
      deepEqual(
          Data.Boolean(false), 
          eq(Data.List([Data.Number(3), Data.List([])]), Data.List([Data.String('3'), Data.List([])])), 
          'nested lists'
      );
      
    });
    
}