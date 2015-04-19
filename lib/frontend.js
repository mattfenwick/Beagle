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
    console.log('maybeCST: ' + JSON.stringify(maybeCST));
    if (maybeCST.status !== 'success') {
        return maybeCST;
    }
    var ast = builder.buildAST(maybeCST.value.result);
    console.log('ast: ' + JSON.stringify(ast));
    return ast;
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

