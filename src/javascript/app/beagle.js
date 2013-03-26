define(["app/tokenizer", "app/parser", "app/evaluate"], function (tokenizer, parser, evaluate) {
    "use strict";

    var env = evaluate.getDefaultEnv();

    function evaler(p) {
        return evaluate.eval(p, env);
    }

    function stripJunk(tokens) {
        return tokens.filter(function(t) {
            return t.tokentype !== 'comment' && t.tokentype !== 'whitespace';
        });
    }

    function beagle(string) {
        if(typeof string !== 'string') {
            throw new Error('input must be of type string');
        }

        var output = {input: string},
            tokens = tokenizer.tokenize(string);
        output.tokenization = tokens.value;
        if(tokens.status !== 'success') {
            output.status = 'token error';
            return output;
        }

        var asts = tokens.fmap(stripJunk).bind(parser.parse.parse); // why not just PParser.parse.parse(tokens.value.map(stripJunk)) ????
        output.asts = asts.value;
        if(asts.status !== 'success') {
            output.status = 'parse error';
            return output;
        }
        var as = asts.value;
        if(as.rest.length !== 0) {
            output.status = 'parse error';
            return output;
        }

        try {
            var vals = as.result.map(evaler);
            output.status = 'success';
            output.results = vals;
        } catch(e) {
            output.status = 'execution error';
            output.results = e;
        }
        return output;
    }

    return {
        exec:  beagle,
        environment: env
    };

});
