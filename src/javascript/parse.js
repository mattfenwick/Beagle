var Parse = (function () {
    "use strict";

    var TOKEN_TYPES = {
        'whitespace'   : /^\s+/,
        'open-paren'   : /^\(/,
        'close-paren'  : /^\)/,
        'open-square'  : /^\[/,
        'close-square' : /^\]/,
        'integer'      : /^\d+/,
        'float'        : /^(?:\d*\.\d+|\d+\.\d*)/,
        'symbol'       : /^[a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\-\?\*\/\!\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\-\?\*\/\!\<\>]*/,
        'comment'      : /^;+(.*)/,        /* because: 1) * is greedy; 2) . doesn't match \n */
        'string'       : /^"([^"]*)"/,
    }
    
    var TOKEN_ORDER = [
        'whitespace', 'open-paren', 'open-square', 'close-paren', 
        'close-square', 'integer', 'float', 'symbol'
        // both 'comment' and 'string' require special handling ... until I can figure out a better way
    ];
    

    function Token(type, value) {
        if(!(type in TOKEN_TYPES)) {
            throw new Error("invalid token type: " + type);
        }
        this.type = type;
        this.value = value;
    }


    function TokenError(message, value) {
        this.message = message;
        this.value = value;
        this.type = 'TokenError';
    }


    TokenError.prototype.toString = function () {
        return this.message + " (from " + this.value + ")";
    }



    // String -> Maybe (Token, String)
    //   where false is the "empty" value
    //   throws a TokenError if a string is started but not stopped
    //   or if the input doesn't match any token definitions
    function nextToken(string) {
        var match, i, name;

        // 0. empty string
        if (string === "") {
            return false;
        }

        // 1. through 6.
        for(i = 0; i < TOKEN_ORDER.length; i++) {
            name = TOKEN_ORDER[i];
            if(match = string.match(TOKEN_TYPES[name])) {
                return {
                    'token': new Token(name, match[0]),
                    'rest': string.substring(match[0].length)
                };
            }
        }

        // 7. comment
        if (match = string.match(TOKEN_TYPES['comment'])) {
            return {
                'token': new Token('comment', match[1]),
                'rest': string.substring(match[0].length)
            };
        }

        // 8. string
        if (string[0] === '"') {
            match = string.match(TOKEN_TYPES['string']);
            if (match) {
                return {
                    'token': new Token('string', match[1]),
                    'rest': string.substring(match[0].length)
                };
            } else {
                throw new TokenError("tokenizer error: end-of-string (\") not found", string);
            }
        }

        throw new TokenError("unexpected tokenizer error:  no tokens match string", string);
    }


    function tokenize(string) {
        var tokens = [],
            next;
        while (next = nextToken(string)) {
            tokens.push(next.token);
            string = next.rest;
        }
        return tokens;
    }


    function stripTokens(tokens) {
        function isNotCommentNorWS(token) {
            return (token.type !== 'comment' && token.type !== 'whitespace');
        }
        return tokens.filter(isNotCommentNorWS);
    }


    // [Token] -> void
    //   - returns nothing if no problems
    //   - throws a SyntaxError 
    //      if a (string or symbol) is immediately followed
    //      by another string or symbol
    function checkTokenSeparation(tokens) {
        var i, type1, type2;
        for (i = 0; i < tokens.length - 1; i++) {
            type1 = tokens[i].type;
            type2 = tokens[i + 1].type;
            if (type1 === 'string' || type1 === 'symbol') {
                if (type2 === 'string' || type2 === 'symbol') {
                    throw new TokenError("found consecutive string/symbol tokens", tokens.slice(i, i + 2));
                }
                // good to go
            }
            // good to go
        }
    }


    return {
        // the data types
        'Token': function (t, v) {
            return new Token(t, v);
        },

        // the helper functions (exported for testing)
        'nextToken': nextToken,

        // the core public functionality
        'tokenize': tokenize,
        'stripTokens': stripTokens,
        'checkTokenSeparation': checkTokenSeparation
    };

})();