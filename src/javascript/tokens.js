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
        'open-special' : /^(,\()/,  // ",("
        'close-special': /^(,\))/,  // ",)"
        'comment'      : /^;(.*)/,  /* because: 1) * is greedy; 2) . doesn't match \n, so it consumes the rest of the line */
        'float'        : /^(\d*\.\d+|\d+\.\d*)/,
        'integer'      : /^(\d+)/,
        'symbol'       : /^([a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\?\/\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\?\/\<\>]*)/,
        'string'       : /^"([^"]*)"/
    };
    
    // try the tokens in a specific order
    // i.e. float must go before integer -- 
    //   otherwise integer matches only a part of what float would
    // string is left out to allow better error reporting
    //   if a string is opened but not closed
    var TOKEN_ORDER = [
        'whitespace',    'open-paren',     'open-square', 
        'close-paren',   'close-square',   'open-curly', 
        'close-curly',   'open-special',   'close-special',
        'comment',       'string',         'float',
        'integer',       'symbol'
    ];


    function Token(type, value, line, column) {
        if(!(type in TOKEN_TYPES)) {
            throw new Error("invalid token type: " + type);
        }
        this.type    =  type;
        this.value   =  value;
        this.line    =  line;
        this.column  =  column;
    }


    function TokenError(message, line, column, rest) {
        this.message  =  message;
        this.line     =  line;
        this.column   =  column;
        this.rest     =  rest;
        this.type     =  'TokenError';
    }


    TokenError.prototype.toString = function () {
        return this.message + " at line " + this.line + ", column " + this.column;
    };
    
    
    function countLCs(string) {
        var lines = 0,
            columns = 0,
            i;
        for(i = 0; i < string.length; i++) {
            if(string[i] === '\n') {
                lines++;
                columns = 0;
            } else {
                columns++;
            }
        }
        return [lines, columns];
    }

    // String -> Maybe (Token, String)
    //   where false is the "empty" value
    //   throws a TokenError if a string is started but not stopped
    //   or if the input doesn't match any token definitions
    function nextToken(string, line, column) {
        var match, i, name, 
            res, count, 
            newLine, newCol;

        // 0. empty string
        if (string === "") {
            return false;
        }
        
        for(i = 0; i < TOKEN_ORDER.length; i++) {
            name = TOKEN_ORDER[i];
            if(match = string.match(TOKEN_TYPES[name])) {
                // all regex matches return a pair:
                //   0: the entire match
                //   1: the 'saved' portion of the match
                count = countLCs(match[0]);
                // if we pass a newline, have to reset the column number
                newLine = line + count[0];
                newCol = ((count[0] > 0) ? 1 : column) + count[1];
                return {
                    'token' : new Token(name, match[1], line, column),
                    'rest'  : string.substring(match[0].length),
                    'line'  : newLine,
                    'column': newCol
                };
            }
        }
        
        if (string[0] === '"') {   // why is this special-cased?  to provide better error reporting
            throw new TokenError("tokenizer error: end-of-string (\") not found", line, column, string);
        }

        throw new TokenError("tokenizer error:  no tokens match input", line, column, string);
    }


    function tokenize(string) {
        var tokens = [],
            next,
            line = 1,
            column = 1;
        while (next = nextToken(string, line, column)) {
            tokens.push(next.token);
            string = next.rest;
            line = next.line;
            column = next.column;
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
        'Token': function (t, v, l, c) {
            return new Token(t, v, l, c);
        },

        'nextToken'   : nextToken,
        'tokenize'    : tokenize,
        'stripTokens' : stripCommentsAndWhitespace
    };

})();