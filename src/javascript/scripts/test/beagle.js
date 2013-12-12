define([
  "app/beagle", 
  "app/ast", 
  "unparse-js/maybeerror"
], function(beagle, ast, me) {

    module("beagle");
    
    test("tokenization error", function() {
        var r = beagle.parse('123 ",doit now');
        deepEqual(me.error({cause: 'tokenization',
                            error: {line: 1, column: 5,
                                    message: 'end-of-string not found',
                                    rest: '",doit now'}}), 
                  r);
    });
    
    test("parse error", function() {
        var r = beagle.parse('123\n [3');
        deepEqual('error', r.status);
        deepEqual('ast parsing', r.value.cause);
    });

});
