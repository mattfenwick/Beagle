var Tokens = (function () {
    "use strict";

    var TOKEN_TYPES = {
        'whitespace'   : /^\s+/,
        'open-paren'   : /^\(/,
        'close-paren'  : /^\)/,
        'open-square'  : /^\[/,
        'close-square' : /^\]/,
        'comment'      : /^;+(.*)/,        /* because: 1) * is greedy; 2) . doesn't match \n */
        'float'        : /^(?:\d*\.\d+|\d+\.\d*)/,
        'integer'      : /^\d+/,
        'symbol'       : /^[a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\-\?\*\/\!\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\-\?\*\/\!\<\>]*/,
        'string'       : /^"([^"]*)"/,
    }
    
    // comment needs special handling
    var PUNCTUATION = ['whitespace', 'open-paren', 'open-square', 'close-paren', 'close-square'];
    
    // float needs to go before integer -- otherwise integer matches only a part of what float would
    //   'string' requires special handling
    var ATOMS = ['float', 'integer', 'symbol'];
    
    var PUNC_OR_WS = /^[\(\)\[\];\s]/; // one of '()[];' or whitespace
    

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


    function nextPunctuation(string) {
        var match, i, name;
        
        for(i = 0; i < PUNCTUATION.length; i++) {
            name = PUNCTUATION[i];
            if(match = string.match(TOKEN_TYPES[name])) {
                return {
                    'token': new Token(name, match[0]),
                    'rest' : string.substring(match[0].length)
                };
            }
        }
        
        if (match = string.match(TOKEN_TYPES['comment'])) {
            return {
                'token': new Token('comment', match[1]),
                'rest': string.substring(match[0].length)
            };
        }
        
        return false;
    }
    
    
    function nextAtom(string) {
        var match, i, name;
        
        for(i = 0; i < ATOMS.length; i++) {
            name = ATOMS[i];
            if(match = string.match(TOKEN_TYPES[name])) {
                return {
                    'token': new Token(name, match[0]),
                    'rest' : string.substring(match[0].length)
                };
            }
        }
        
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
        
        return false;
    }

    // String -> Maybe (Token, String)
    //   where false is the "empty" value
    //   throws a TokenError if a string is started but not stopped
    //   or if the input doesn't match any token definitions
    function nextToken(string) {
        var match, i, name, res;

        // 0. empty string
        if (string === "") {
            return false;
        }

        if (res = nextPunctuation(string)) {
            return res;
        }
        
        if (res = nextAtom(string)) {
            if(res.rest === "" || res.rest.match(PUNC_OR_WS)) {
                return res;
            } else {
                throw new TokenError("atom must be followed by whitespace or punctuation", res);
            }
        }

        // is it really unexpected to get here? ????
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


    return {
        // the data types
        'Token': function (t, v) {
            return new Token(t, v);
        },

        // the helper functions (exported for testing)
        'nextToken': nextToken,

        // the core public functionality
        'tokenize': tokenize,
        'stripTokens': stripTokens
    };

})();