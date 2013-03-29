define(["app/tokenizer", "app/parser", "app/evaluate", "libs/maybeerror"], function (tokenizer, parser, evaluate, me) {
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
    
    function beagle(input) {
        // success:  maybeerror.pure({input: ..., tokens: ..., asts: ..., results: ...});
        // error:  maybeerror.error({cause: 'tokenization'/'parsing'/'evaluation', error: {...}})
        if(typeof input !== 'string') {
            throw new Error('input must be of type string');
        }
        
        var tokens, asts, results;
        
        tokens = tokenizer.tokenize(input);
        if ( tokens.status !== 'success' ) {
            return me.error({cause: 'tokenization', error: tokens.value});
        }
        
        asts = parser.parse.parse(stripJunk(tokens.value));
        if ( asts.status !== 'success' ) {
            return me.error({cause: 'ast parsing', error: asts.value});
        }
        
        // assumes: parsing consumes entire input and succeeds,
        //   or fails with an error
        
        try {
            results = asts.value.result.map(evaler);
        } catch (e) {
            return me.error({cause: 'evaluation', error: e});
        }
        
        return me.pure({'tokens': tokens.value, 'asts': asts.value.result, 'results': results});
    }
    
    return {
        exec:  beagle,
        environment: env
    };

});
