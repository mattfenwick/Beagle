
function testEvaluate(evaluate) {

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
          
      deepEqual({'type': 'number', 'value': 345}, evaluate.makePrimitives(int_));
          
      try {
        var e = evaluate.makePrimitives(empty);
        ok(0);
      } catch(e) {
        ok(1, "can't make primitive of empty string");
      };
          
      deepEqual({'type': 'number', 'value': 3}, evaluate.makePrimitives(float1));
          
      deepEqual({'type': 'number', 'value': 3.456}, evaluate.makePrimitives(float2));
          
      deepEqual({'type': 'number', 'value': 0.001}, evaluate.makePrimitives(float3));
          
      deepEqual({'type': 'number', 'value': 0.01}, evaluate.makePrimitives(float4));
          
      deepEqual({'type': 'string', 'value': "yes this is a string"}, evaluate.makePrimitives(str));
          
      deepEqual({'type': 'symbol', 'value': '"open'}, evaluate.makePrimitives(notstr));
          
      deepEqual({'type': 'symbol', 'value': '*?#""/'}, evaluate.makePrimitives(sym1));
          
      var l1 = {
        'type': 'list', 
        'value': [
          {'type': 'symbol', 'value': '+'},
          {'type': 'string', 'value': 'str1'},
          {'type': 'number', 'value': 345}
        ]
      };
      deepEqual(l1, evaluate.makePrimitives(list1));
          
      var l2 = {
        'type': 'list', 
        'value': [
          {'type': 'symbol', 'value': '+'},
          {'type': 'list', 'value': [
            {'type': 'symbol', 'value': '-'},
            {'type': 'number', 'value': 34.32}
          ]},
          {'type': 'symbol', 'value': '"omg'}
        ]
      };          
      deepEqual(l2, evaluate.makePrimitives(list2));
    });

}