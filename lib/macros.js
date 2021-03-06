'use strict';

var Parser = require('./parser');

function Let(bindings, body) {
    this.bindings = bindings;
    this.body = body;
}

// Macro -> AST
Let.prototype.expand = function() {
    // f :: [(Sym, Expr)] -> [Define]
    function f(b) {
        return Parser.Define(b.elements);//inding);
    }
    return Parser.Application([
        Parser.Lambda([Parser.ASTList([])].concat(this.bindings.map(f), this.body))
    ]);
};

module.exports = {
    'Let': function(bs, b) {
        return new Let(bs, b);
    }
};

