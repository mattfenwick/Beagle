"use strict";

var parser = require('./parser'),
    builder = require('./builder'),
    fs = require('fs');
    
function readFile(path) {
    return fs.readFileSync('beagle/test.bgl', 'utf-8');
}

function parse(text) {
    return parser.beagle.parse(text, [1, 1]);
}

function buildAST(cst) {
    return builder.buildAST(cst);
}

function parseAST(text) {
    var maybeCST = parse(text);
    if (maybeCST.status === 'success') {
        return buildAST(maybeCST.value.result);
    }
    return maybeCST;
}

function parseASTFromFile(path) {
    var text = readFile(path); // TODO of course, this could fail
    return parseAST(text);
}

module.exports = {
    'parse'     : parse     ,
    'buildAST'  : buildAST  ,
    'parseAST'  : parseAST  ,
    'parseASTFromFile': parseASTFromFile
};

