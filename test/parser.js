'use strict';

var P = require('../lib/parser'),
    M = require('unparse-js').maybeerror,
    assert = require("assert");

var module = describe,
    test = it,
    deepEqual = assert.deepEqual;

module("parser", function() {
    
    function number(pos, int) {
        return {'_name': 'number', '_state': pos, 'int': int};
    }
    
    var pure = M.pure,
        error = M.error,
        empty = {},
        form = P.form,
        on1 = number([1,1], ['0', '0', '4']);
        
    function good(value, rest, state) {
        return pure({'result': value, 'rest': rest, 'state': state});
    }
    
    test("number", function() {
        deepEqual(form.parse('004', [1,1]), good(on1, '', [1,4]));
        deepEqual(form.parse('1321ab', [3,2]), good(number([3,2], '1321'.split('')), 'ab', [3,6]));
    });
    
/*    test("string", function() {
    
    });
    
    test("symbol", function() {
    
    });
*/    
/*    test("list", function() {
        deepEqual(form.parse('[]', [1,1]), good({}, '', [1,3]));
        deepEqual(form.parse('[a 123]', [1,1]), good({}, '', [1,8]));
        deepEqual(form.parse('[', [1,1]), error({}));
    });
/*
/*    
    test("application", function() {
        deepEqual(form.parse('(f)', [1,1]), good({}, '', [1,3]));
        deepEqual(form.parse('(f x 2)', [1,1]), good({}, '', [1,3]));
        deepEqual(form.parse('()', [1,1]), error({}));
        deepEqual(form.parse('(f 3', [1,1]), error({}));
    });
    
    test("special forms: define", function() {
        deepEqual(form.parse('{define x 3}', [1,1]), error({}));
        deepEqual(form.parse('{define y 4', [1,1]), error({}));
        deepEqual(form.parse('{define z', [1,1]), error({}));
        deepEqual(form.parse('{define 3 x}', [1,1]), error({}));
    });
    
    test("special forms: set", function() {
        deepEqual(form.parse('{set x 3}', [1,1]), error({}));
        deepEqual(form.parse('{set y 4', [1,1]), error({}));
        deepEqual(form.parse('{set z', [1,1]), error({}));
        deepEqual(form.parse('{set 3 x}', [1,1]), error({}));
    });
    
    test("special forms: lambda", function() {
        deepEqual(form.parse('{lambda {x y} z}', [1,1]), error({}));
        deepEqual(form.parse('{lambda {x y} }', [1,1]), error({}));
        deepEqual(form.parse('{lambda {x y} z', [1,1]), error({}));
        deepEqual(form.parse('{lambda (x y) z}', [1,1]), error({}));
        deepEqual(form.parse('{lambda {3} z}', [1,1]), error({}));
        deepEqual(form.parse('{lambda z}', [1,1]), error({}));
    });
    
    test("special forms: cond", function() {
        deepEqual(form.parse('{cond {{x 3}} 4}', [1,1]), error({}));
        deepEqual(form.parse('{cond {} 5}', [1,1]), error({}));
        deepEqual(form.parse('{cond {} 5', [1,1]), error({}));
        deepEqual(form.parse('{cond {}}', [1,1]), error({}));
        deepEqual(form.parse('{cond {x 3} 5}', [1,1]), error({}));
        deepEqual(form.parse('{cond {{x}} 6}', [1,1]), error({}));
        deepEqual(form.parse('{cond {{x y z}} 7}', [1,1]), error({}));
        deepEqual(form.parse('{cond 5}', [1,1]), error({}));
    });
    
    test("special forms", function() {        
        deepEqual(form.parse('{blar x 3}', [1,1]), error({}));
    });
*/
});

