'use strict';

var F = require('../lib/frontend'),
    util = require('util'),
    assert = require("assert");

var module = describe,
    test = it,
    deepEqual = assert.deepEqual;

module("frontend", function() {

    test("symbol", function() {
        var parsed = F.parseAST('xyz');
        console.log('parsed: ' + util.inspect(parsed, {'depth': null}));
        deepEqual(parsed.status, 'success');
    });

/*
    test("simple file", function() {
        var parsed = F.parseASTFromFile('beagle/test.bgl');
        console.log('parsed: ' + util.inspect(parsed, {'depth': null}));
        deepEqual(parsed.status, 'success');
    });
/**/
});

