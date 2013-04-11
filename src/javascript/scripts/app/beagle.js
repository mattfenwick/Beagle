define(["app/tokenizer", "app/parser", "libs/maybeerror"], function (tokenizer, parser, me) {
    "use strict";

    function stripJunk(tokens) {
        return tokens.filter(function(t) {
            return t.tokentype !== 'comment' && t.tokentype !== 'whitespace';
        });
    }
    
    function parse(input) {
        // success:  maybeerror.pure(asts...);
        // error:  maybeerror.error({cause: 'tokenization'/'parsing', error: {...}})
        if(typeof input !== 'string') {
            throw new Error('input must be of type string');
        }
        
        var tokens, asts;
        
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
        
        return me.pure(asts.value.result); // ditch the rest/state wrapper
    }
    
    return {
        'stripJunk': stripJunk,
        'parse'    : parse
    };

});
