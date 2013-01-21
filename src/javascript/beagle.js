var Beagle = (function (tokens, parser, evaluate) {
    "use strict";

    var env = evaluate.getDefaultEnv();


    function evaler(p) {
        return evaluate.eval(p, env);
    }


    // String -> [SExpression]
    //   returns a list of s-expressions,
    //   throws if strings and symbols aren't properly separated
    //   throws if there are still tokens left in the token stream
    function getTokens(str) {
        var allTokens = tokens.tokenize(str),
            toks;

        // remove whitespace and comment tokens
        toks = tokens.stripTokens(allTokens);

        return toks;
    }


    function exec(str) {
        var toks = getTokens(str),
            trees = parser.makeAST(toks),
            evaled = trees.map(evaler);

        return {
            'string': str,
            'result': evaled,
            'tokens': toks,
            'ast'   : trees
        };
    }


    return {
        'exec'        : exec,
        'environment' : env,
        'getTokens'   : getTokens
    };

})(Tokens, PParser, Evaluate);