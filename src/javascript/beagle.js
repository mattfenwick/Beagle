var Beagle = (function (parse, evaluate) {
    "use strict";

    var env = evaluate.getDefaultEnv();


    function primMaker(sexpr) {
        return evaluate.makePrimitives(sexpr);
    }


    function evaler(p) {
        return evaluate.eval(p, env);
    }


    // String -> [SExpression]
    //   returns a list of s-expressions,
    //   throws if strings and symbols aren't properly separated
    //   throws if there are still tokens left in the token stream
    function parseString(str) {
        var allTokens = parse.tokenize(str),
            tokens, sexprs;

        // throws an exception if any problems found
        parse.checkTokenSeparation(allTokens);

        // otherwise keep going if tokens are fine
        // remove whitespace and comment tokens
        tokens = parse.stripTokens(allTokens);

        // throws an exception if anything weird in 'tokens'
        sexprs = parse.makeSExpressions(tokens);

        return sexprs;
    }


    function exec(str) {
        var results = parseString(str),
            prims = results.map(primMaker),
            evaled = prims.map(evaler);

        return {
            'string': str,
            'result': evaled,
            'parsed': results,
            'primitives': prims
        };
    }


    return {
        'exec': exec,
        'environment': env,
        'parseString': parseString
    };

})(Parse, Evaluate);