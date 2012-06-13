
function testEnvironment(environment, data) {

    if( !environment || !data ) {
      throw new Error("can't run tests without dependencies");
    }

    module("built-in functions");

    test("cons, list, car, cdr", function() {
    
      deepEqual(3, environment.car.value(data.List([3, 4])));
    
      // uh-oh, empty list!
      deepEqual(data.Nil(), environment.car.value(data.List([])));
    
      deepEqual(
          ev.List([4, 10, 'hello']), 
          environment.cdr.value(ev.List([3, 4, 10, 'hello']))
      );
    
      // uh-oh !!
      deepEqual(data.Nil(), environment.cdr.value(data.List([])));
      
      deepEqual(data.List([3, 4, 5]), environment.list.value(3, 4, 5));
      
    });
    
}