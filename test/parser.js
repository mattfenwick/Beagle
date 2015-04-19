'use strict';

var P = require('../lib/parser'),
    fs = require('fs'),
    util = require('util'),
    assert = require("assert");

var module = describe,
    test = it,
    deepEqual = assert.deepEqual;

module("parser -- real code", function() {

    test("test.bgl", function() {
        var text = fs.readFileSync('beagle/test.bgl', 'utf-8'),
            parsed = P.beagle.parse(text, [1,1]);
        console.log(util.inspect(parsed, {'depth': null}));
        deepEqual(parsed.status, 'success');
    });

});

