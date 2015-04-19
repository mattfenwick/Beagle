'use strict';

var P = require('../lib/parser'),
    B = require('../lib/builder'),
    fs = require('fs'),
    util = require('util'),
    assert = require("assert");

var module = describe,
    test = it,
    deepEqual = assert.deepEqual;

module("builder", function() {

    test("symbol", function() {
        var parsed = P.beagle.parse('xyz', [1,1]);
        console.log('parsed: ' + util.inspect(parsed, {'depth': null}));
        var cst = parsed.value.result[0];
        console.log('cst: ' + util.inspect(cst, {'depth': null}));
        var ast = B.buildAST(cst);
        console.log('ast: ' + util.inspect(ast, {'depth': null}));
        deepEqual(ast.status, 'success');
    });

});

