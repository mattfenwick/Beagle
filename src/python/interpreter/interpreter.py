'use strict';

var core = require('./core');

function interpret(ast) {
    return core.evaluate(ast, core.getDefaultEnv());
}


module.exports = {
    'interpret': interpret
};

