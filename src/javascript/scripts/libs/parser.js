define(["libs/maybeerror", "libs/parsercombinators"], function(MaybeError, ParserFactory) {
    "use strict";

    var Parser = ParserFactory(MaybeError);
    
    return Parser;

});