
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
    
}