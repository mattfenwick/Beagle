
function testEvaluate(evaluate, funcs, data) {

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
          
      deepEqual(data.Number(345), ev.makePrimitives(int_));
          
      try {
        var e = ev.makePrimitives(empty);
        ok(0);
      } catch(e) {
        ok(1, "can't make primitive of empty string");
      };
          
      deepEqual(data.Number(3), ev.makePrimitives(float1));
          
      deepEqual(data.Number(3.456), evaluate.makePrimitives(float2));
          
      deepEqual(data.Number(0.001), evaluate.makePrimitives(float3));
          
      deepEqual(data.Number(0.01), evaluate.makePrimitives(float4));
          
      deepEqual(data.String("yes this is a string"), ev.makePrimitives(str));
          
      deepEqual(data.Symbol('"open'), evaluate.makePrimitives(notstr));
          
      deepEqual(data.Symbol('*?#""/'), evaluate.makePrimitives(sym1));
          
      var l1 = data.List([data.Symbol('+'), data.String('str1'), data.Number(345)]);
      deepEqual(l1, ev.makePrimitives(list1));
          
      var l2 = data.List([
          data.Symbol('+'),
          data.List([data.Symbol('-'), data.Number(34.32)]),
          data.Symbol('"omg')
      ]);
      deepEqual(l2, evaluate.makePrimitives(list2));
    });
    
    
    
    test("evaluate", function() {
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
          
      deepEqual(data.Number(31), evaluate.eval(int_));
          
      deepEqual(data.String("abcde"), evaluate.eval(str));
          
      deepEqual(data.Function(funcs.cons), evaluate.eval(sym));
    
      deepEqual(data.List([]), ev.eval(data.List([data.Symbol('list')])));
    
      deepEqual(data.List([data.Number(4)]), ev.eval(data.List([data.Symbol('list'), data.Number(4)])));
          
      deepEqual(
        data.Number(87),
        evaluate.eval(l1)
      );
          
      deepEqual(data.List([data.String('what?')]), evaluate.eval(l2));
      
    });

}