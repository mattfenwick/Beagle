function testParseTree(pt, helper) {

    module("parse tree");
    
    var exp = helper.expectException;

    test("number", function() {
        deepEqual({type: 'parsenode', nodetype: 'number', value: 13, meta: 'abc'}, pt.number(13, 'abc'));
    
        exp(function() {
            pt.number('oops');
        }, 'TypeError');
    });

    test("string", function() {
        deepEqual({type: 'parsenode', nodetype: 'string', value: 'hi', meta: 4}, pt.string('hi', 4));
    
        exp(function() {
            pt.string([1]);
        }, 'TypeError');
    });

    test("symbol", function() {
        deepEqual({type: 'parsenode', nodetype: 'symbol', value: 'hi', meta: '??'}, pt.symbol('hi', '??'));
    
        exp(function() {
            pt.symbol({a: 4});
        }, 'TypeError');
    });

    test("list literal", function() {
        deepEqual({type: 'parsenode', nodetype: 'listliteral', elements: [1,2,3], meta: 'hi'}, pt.list([1,2,3], 'hi'));
        
        exp(function() {
            pt.list(3);
        }, 'TypeError');
        
        exp(function() {
            pt.list('qrs');
        }, 'TypeError', 'detect difference between array and string');
    });

    test("application", function() {
        deepEqual(
            {type: 'parsenode', nodetype: 'application', 
             operator: pt.symbol('qrs'), 'arguments': [], meta: 2}, 
            pt.app(pt.symbol('qrs'), [], 2)
        );
        
        exp(function() {
            pt.app(3, [], 2);
        }, 'TypeError', 'application operator must be a parsetree node');
        
        exp(function() {
            pt.app(pt.symbol('qrs'), 3);
        }, 'TypeError', 'application arguments must be an array');
    });

    test("object literal", function() {
        deepEqual(
            {type: 'parsenode', nodetype: 'objectliteral',
             entries: [['abc', 123]], meta: {q: 'rs'}},
            pt.object([['abc', 123]], {q: 'rs'})
        );
        
        exp(function() {
            pt.object(123);
        }, 'TypeError', 'needs array ...');
        
        exp(function() {
            pt.object([123]);
        }, 'TypeError', '... of arrays ... ');
        
        exp(function() {
            pt.object([[1]]);
        }, 'ValueError', '... of length 2 ...');
        
        exp(function() {
            pt.object([[123, 'abc']]);
        }, 'TypeError', '... where the first element is a string');
    });

    test("special form", function() {
        deepEqual(
            {type: 'parsenode', nodetype: 'special',
             operator: 'cond', 'arguments': [], meta: 4},
            pt.special('cond', [], 4)
        );
        
        exp(function() {
            pt.special({}, [], 41);
        }, 'TypeError', 'special form operator must be a string');
        
        exp(function() {
            pt.special('define', true, 'me');
        }, 'TypeError', 'special form arguments must be an array');
    });

}