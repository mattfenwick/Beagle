
function testEvaluate(evaluate) {

    var ev = evaluate;

    module("primitives");

    test("makePrimitives", function() {
      var int_ = "345",
          empty = "",
          float1 = "03.",
          float2 = "3.456",
          float3 = ".001",
          float4 = "0.01",
          str = '"yes this is a string"',
          notstr = '"open',
          sym1 = '*?#""/',
          list1 = ["+", '"str1"', "345"],
          list2 = ["+", ["-", "34.32"], '"omg'];
          
      deepEqual(ev.Number(345), ev.makePrimitives(int_));
          
      try {
        var e = ev.makePrimitives(empty);
        ok(0);
      } catch(e) {
        ok(1, "can't make primitive of empty string");
      };
          
      deepEqual(ev.Number(3), ev.makePrimitives(float1));
          
      deepEqual(ev.Number(3.456), evaluate.makePrimitives(float2));
          
      deepEqual(ev.Number(0.001), evaluate.makePrimitives(float3));
          
      deepEqual(ev.Number(0.01), evaluate.makePrimitives(float4));
          
      deepEqual(ev.String("yes this is a string"), ev.makePrimitives(str));
          
      deepEqual(ev.Symbol('"open'), evaluate.makePrimitives(notstr));
          
      deepEqual(ev.Symbol('*?#""/'), evaluate.makePrimitives(sym1));
          
      var l1 = ev.List([ev.Symbol('+'), ev.String('str1'), ev.Number(345)]);
      deepEqual(l1, ev.makePrimitives(list1));
          
      var l2 = ev.List([
          ev.Symbol('+'),
          ev.List([ev.Symbol('-'), ev.Number(34.32)]),
          ev.Symbol('"omg')
      ]);
      deepEqual(l2, evaluate.makePrimitives(list2));
    });
    
    
    

    module("evaluate");

    test("cons, list, car, cdr", function() {
    
      deepEqual(3, evaluate.environment.car.value(ev.List([3, 4])));
    
      // uh-oh, empty list!
      deepEqual(ev.Nil(), evaluate.environment.car.value(ev.List([])));
    
      deepEqual(
          ev.List([4, 10, 'hello']), 
          evaluate.environment.cdr.value(ev.List([3, 4, 10, 'hello']))
      );
    
      // uh-oh !!
      deepEqual(ev.Nil(), evaluate.environment.cdr.value(ev.List([])));
      
      deepEqual(ev.List([3, 4, 5]), ev.environment.list.value(3, 4, 5));
      
    });
    
    test("evaluate", function() {
      var int_ = ev.Number(31),
          str = ev.String("abcde"),
          sym = ev.Symbol('cons'),
          l1 = ev.List([
              ev.Symbol('car'),
              ev.List([ev.Symbol('list'), ev.Number(87)])
          ]),
          l2 = ev.List([
              ev.Symbol('cons'),
              ev.String('what?'),
              ev.List([ev.Symbol('list')])
          ])
          ;
          
      deepEqual(ev.Number(31), evaluate.eval(int_));
          
      deepEqual(ev.String("abcde"), evaluate.eval(str));
          
      deepEqual(ev.Function(evaluate.environment.cons.value), evaluate.eval(sym));
    
      deepEqual(ev.List([]), ev.eval(ev.List([ev.Symbol('list')])));
    
      deepEqual(ev.List([ev.Number(4)]), ev.eval(ev.List([ev.Symbol('list'), ev.Number(4)])));
          
      deepEqual(
        ev.Number(87),
        evaluate.eval(l1)
      );
          
      deepEqual(ev.List([ev.String('what?')]), evaluate.eval(l2));
      
    });

}