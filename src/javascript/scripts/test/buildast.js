define([
  "app/ast",
  "app/buildast"
], function(A, BA) {

    module("buildast");
    
    function number(pos, int, dec) {
        return {'_name': 'number', '_state': pos, 'int': int, 'decimal': dec};
    }
    
    function char(pos, type, c) {
        return {'_name': type, '_state': pos, 'char': c};
    }
    
    return function () {

        var in1 = number(3, ['1', '4'], null),
            is1 = {'_name': 'string', '_state': 'hi', 'body': []},
            is2 = {'_name': 'string', '_state': 13, 
                   'body': [char(4, 'simple', 'x'), char(9, 'escape', '"')]},
            iy1 = {'_name': 'symbol', '_state': false, 'first': 'a', 'rest': []},
            iy2 = {'_name': 'symbol', '_state': true, 'first': '_', 'rest': ['m', '3', '%']},
            il1 = {'_name': 'list', '_state': 4, 'body': []},
            il2 = {'_name': 'list', '_state': 34, 'body': [in1, is2]},
            ia1 = {'_name': 'app', '_state': 2, 'operator': is1, 'args': []},
            ia2 = {'_name': 'app', '_state': '3', 'operator': is2, 'args': [in1, il1]},
            on1 = A.number(14, 3),
            os1 = A.list([], 'hi'),
            os2 = A.list([A.char('x'), A.char('"')], 13),
            oy1 = A.symbol('a', false),
            oy2 = A.symbol('_m3%', true),
            ol1 = A.list([], 4),
            ol2 = A.list([on1, os2], 34),
            oa1 = A.application(os1, [], 2),
            oa2 = A.application(os2, [on1, ol1], '3');
            
        test("number", function() {
            deepEqual(BA.build(in1), on1);
        });
        
        test("string", function() {
            deepEqual(BA.build(is1), os1);
            deepEqual(BA.build(is2), os2);
        });
        
        test("symbol", function() {
            deepEqual(BA.build(iy1), oy1);
            deepEqual(BA.build(iy2), oy2);
        });
        
        test("list", function() {
            deepEqual(BA.build(il1), ol1);
            deepEqual(BA.build(il2), ol2);
        });
        
        test("app", function() {
            deepEqual(BA.build(ia1), oa1);
            deepEqual(BA.build(ia2), oa2);
        });
        
        test("special forms -- lambda, define, cond, set", function() {
            deepEqual(true, false);
        });

    };
    
});
