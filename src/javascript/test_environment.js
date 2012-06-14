
function testEnvironment(environment, data) {

    if( !environment || !data ) {
      throw new Error("can't run tests without dependencies");
    }

    module("built-in functions");

    test("cons, list, car, cdr", function() {
    
      var car = environment.getBinding('car').value;
      deepEqual(3, car(data.List([3, 4])));
    
      // uh-oh, empty list!
      deepEqual(data.Nil(), car(data.List([])));
    
      var cdr = environment.getBinding('cdr').value;
      deepEqual(
          data.List([4, 10, 'hello']), 
          cdr(data.List([3, 4, 10, 'hello']))
      );
    
      // uh-oh !!
      deepEqual(data.Nil(), cdr(data.List([])));
      
      var list = environment.getBinding('list').value;
      deepEqual(data.List([3, 4, 5]), list(3, 4, 5));
      
    });
    
}