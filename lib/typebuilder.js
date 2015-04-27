'use strict';


function makeTypes(typeDefs, typekey) {
    var rootNode     = Object.create({}),
        types        = {},
        constructors = {};

    function makeType(typename, argnames) {
        var type = Object.create(rootNode);
//        type[typekey] = typename;  // uncomment if you can figure out how to get prototype properties to show up in util.inspect
        var constructor = function typeConstructor() {
                var args = Array.prototype.slice.call(arguments);
                if (args.length !== argnames.length) {
                    throw new Error('invalid number of arguments -- expected ' + argnames.length + ', got ' + args.length);
                }
                var self = Object.create(type);
                for (var i = 0; i < args.length; i++) {
                    self[argnames[i]] = args[i];
                }
                self[typekey] = typename; // this is just to get it to show up in util.inspect, otherwise it'd go in the prototype
                // TODO what about overrides?  should I worry about reporting/avoiding those?
                return self;
            };
        types[typename] = type;
        constructors[typename] = constructor;
    }
    
    typeDefs.forEach(function (typeDef) {
        makeType(typeDef[0], typeDef[1]);
    });

    return {
        'rootNode'      : rootNode      ,
        'types'         : types         ,
        'constructors'  : constructors  ,
    };
}

module.exports = {
    'makeTypes': makeTypes
};

