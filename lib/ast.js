'use strict';

var Typebuilder = require('./typebuilder');

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

var typesAndCons = Typebuilder.makeTypes(typeDefs, 'asttype');


module.exports = {
    'rootASTNode'   : typesAndCons.rootNode,    
    'types'         : typesAndCons.types,
    'constructors'  : typesAndCons.constructors
};

