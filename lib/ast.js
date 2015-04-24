'use strict';


var rootASTNode = Object.create({}),
    types = {},
    constructors = {};

function makeType(typename, argnames) {
    var type = Object.create(rootASTNode, {'type': {'value': typename}}), // TODO can I make this be enumerated?
        constructor = function typeConstructor() {
            var args = Array.prototype.slice.call(arguments);
            if (args.length !== argnames.length) {
                throw new Error('invalid number of arguments');
            }
            var self = Object.create(type);
            for (var i = 0; i < args.length; i++) {
                self[argnames[i]] = args[i];
            }
            // TODO what about overrides?  should I worry about reporting/avoiding those?
            return self;
        }
    types[typename] = type;
    constructors[typename] = constructor;
}

var typeDefs = [
    ['Number',      ['numValue'             ]],
    ['Symbol',      ['strValue'             ]],
    ['String',      ['strValue'             ]],
    ['List',        ['elems'                ]],
    ['Dictionary',  ['keyvals'              ]],
    ['Application', ['operator', 'arguments']],
    ['Def',         ['symbol', 'value'      ]],
    ['Set',         ['symbol', 'value'      ]],
    ['Cond',        ['branches', 'elseValue']],
    ['Fn',          ['params', 'forms'      ]],
    ['Beagle',      ['forms'                ]]
];

typeDefs.forEach(function (typeDef) {
    makeType(typeDef[0], typeDef[1]);
});


module.exports = {
    'rootASTNode'   : rootASTNode,
    'makeType'      : makeType,
    
    'types'         : types,
    'constructors'  : constructors
};

