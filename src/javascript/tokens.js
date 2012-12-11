var Tokens = (function () {
    "use strict";

    // put capturing parentheses around that which should be saved
    //   for *all* tokens; don't worry about efficiency
    var TOKEN_TYPES = {
        'whitespace'   : /^(\s+)/,
        'open-paren'   : /^(\()/,   // "("
        'close-paren'  : /^(\))/,   // "("
        'open-square'  : /^(\[)/,   // "["
        'close-square' : /^(\])/,   // "]"
        'open-curly'   : /^(\{)/,   // "{"
        'close-curly'  : /^(\})/,   // "}"
        'open-special' : /^(,\()/,  // "~("
        'close-special': /^(,\))/,  // "~)"
        'comment'      : /^;(.*)/,        /* because: 1) * is greedy; 2) . doesn't match \n */
        'float'        : /^(\d*\.\d+|\d+\.\d*)/,
        'integer'      : /^(\d+)/,
        'symbol'       : /^([a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\?\/\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\?\/\<\>]*)/,
        'string'       : /^"([^"]*)"/,
    }
    
    // try the tokens in a specific order
    // i.e. float must go before integer -- 
    //   otherwise integer matches only a part of what float would
    // string is left out to allow better error reporting
    //   if a string is opened but not closed
    var TOKEN_ORDER = [
        'whitespace',    'open-paren',     'open-square', 
        'close-paren',   'close-square',   'open-curly', 
        'close-curly',   'open-special',   'close-special',
        'comment',
        'float',         'integer',        'symbol'];


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
    };
    

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
        
        for(i = 0; i < TOKEN_ORDER.length; i++) {
            name = TOKEN_ORDER[i];
            if(match = string.match(TOKEN_TYPES[name])) {
                // all matches should return a pair:
                //   0: the entire match;  1: the 'saved' portion of the match
                return {
                    'token': new Token(name, match[1]),
                    'rest' : string.substring(match[0].length)
                };
            }
        }
        
        if (string[0] === '"') {   // why is this special-cased?  to provide better error reporting
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


    function stripCommentsAndWhitespace(tokens) {
        function predicate(token) {
            return (token.type !== 'comment' && token.type !== 'whitespace');
        }
        return tokens.filter(predicate);
    }


    return {
        'Token': function (t, v) {
            return new Token(t, v);
        },

        'nextToken'   : nextToken,
        'tokenize'    : tokenize,
        'stripTokens' : stripCommentsAndWhitespace
    };

})();