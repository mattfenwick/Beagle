define([
  "app/data"
], function(data) {
	
    return function () {

    	module("data");
    	
    	test("number", function() {
    		propEqual({type: 'number', value: 3}, data.Number(3));
    	});
        
        test("char", function() {
            propEqual({type: 'char', value: 'a'}, data.Char('a'));
        });
        
        test("list", function() {
            propEqual({type: 'list', value: [data.Number(4)]}, data.List([data.Number(4)]));
        });
        
        test("function", function() {
            propEqual({argTypes: ['string'], name: 'alert', type: 'function', value: alert}, data.Function(['string'], 'alert', alert));
        });
        
        test("null", function() {
            propEqual({type: 'null'}, data.Null());
        });
        
        test("boolean", function() {
            propEqual({type: 'boolean', value: true}, data.Boolean(true));
        });

    };
    
});
