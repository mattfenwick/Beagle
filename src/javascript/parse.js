var Parse = (function () {
    "use strict";

    var TOKEN_TYPES = {
        'whitespace'   : /^\s+/,
        'open-paren'   : /^\(/,
        'close-paren'  : /^\)/,
        'open-square'  : /^\[/,
        'close-square' : /^\]/,
        'symbol'       : /^[^;\s\(\)"]+/,  /* not ;, whitespace, (, ), " */
        'comment'      : /^;+(.*)/,        /* because: 1) * is greedy; 2) . doesn't match \n */
        'string'       : /^"([^"]*)"/,
    }
    
    var TOKEN_ORDER = [
        'whitespace', 'open-paren', 'open-square', 'close-paren', 
        'close-square', 'symbol' // both 'comment' and 'string' require special handling ... until I can figure out a better way
    ];
    

    function Token(type, value) {
        if(!(type in TOKEN_TYPES)) {
            throw new Error("invalid token type: " + type);
        }
        this.type = type;
        this.value = value;
    }


    // SEexpression types: symbol, string, list
    function SExpression(type, value) {
        this.type = type;
        this.value = value;
    }


    function ParseError(message, value) {
        this.message = message;
        this.value = value;
        this.type = 'ParseError';
    }


    ParseError.prototype.toString = function () {
        return this.message + " (from " + this.value + ")";
    }



    // String -> Maybe (Token, String)
    //   where false is the "empty" value
    //   throws a ParseError if a string is started but not stopped
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
                throw new ParseError("tokenizer error: end-of-string (\") not found", string);
            }
        }

        throw new ParseError("unexpected tokenizer error:  no tokens match string", string);
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
    


    // [Token] -> Maybe SExpression
    //   returns false if token stream is empty or first token is NOT a symbol/string
    function getAtom(tokens) {
        if (tokens.length === 0) {
            return false;
        }

        var first = tokens[0];

        if (first.type === 'symbol' || first.type === 'string') {
            return {
                result: new SExpression(first.type, first.value),
                rest: tokens.slice(1)
            };
        }

        return false;
    }


    // [Token] -> Maybe SExpression
    //   returns false if tokens is empty or doesn't start with open-paren
    //   throws an error if first token is a ')'
    //   throws an error if a properly 'balanced' list can't be found
    function getList(tokens) {
        var sexpr, elems = [],
            inputTokens = tokens;

        if (tokens.length === 0) {
            return false;
        }

        if (tokens[0].type === 'close-paren') {
            throw new ParseError("')' token found without matching '('", inputTokens);
        }

        // a list *has* to start with a '('
        if (tokens[0].type !== 'open-paren') {
            return false;
        }

        tokens = tokens.slice(1);

        // keep going until a ')'
        while (tokens[0] && (tokens[0].type !== 'close-paren')) {
            // a list could have as many nested lists or atoms as it pleased
            sexpr = getSExpression(tokens);
            if (!sexpr) {
                return false;
            }
            elems.push(sexpr.result);
            tokens = sexpr.rest;
        }

        // a list must end with a ')'
        if (tokens[0] && tokens[0].type === 'close-paren') {
            return {
                result: new SExpression('list', elems),
                rest: tokens.slice(1)
            };
        }

        // uh-oh!  we didn't find a close-paren ...
        throw new ParseError("'(' token found without matching ')'", inputTokens);
    }


    function getSExpression(tokens) {
        var sexpr;

        if (tokens.length === 0) {
            return false;
        }

        // an s-expression is either an atom
        sexpr = getAtom(tokens);
        if (sexpr) {
            return sexpr;
        }

        // or a list
        sexpr = getList(tokens);
        if (sexpr) {
            return sexpr;
        }

        // no other possibilities
        throw new ParseError("unexpected error:  couldn't find s-expression and token stream was not empty", tokens);
    }


    function stripTokens(tokens) {
        function isNotCommentNorWS(token) {
            return (token.type !== 'comment' && token.type !== 'whitespace');
        }
        return tokens.filter(isNotCommentNorWS);
    }


    // assumes the tokens are of type 'string', 'symbol', 'open-paren', and 'close-paren'
    //   anything else should throw an exception
    function makeSExpressions(tokens) {
        var sexprs = [],
            sexpr;

        while (sexpr = getSExpression(tokens)) {
            sexprs.push(sexpr.result);
            tokens = sexpr.rest;
        }

        if (tokens.length !== 0) {
            throw new ParseError('unconsumed tokens remaining after parse', tokens);
        }

        return sexprs;
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
                    throw new ParseError("found consecutive string/symbol tokens", tokens.slice(i, i + 2));
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
        'SExpression': function (t, v) {
            return new SExpression(t, v);
        },
        'ParseError': function (m, v) {
            return new ParseError(m, v);
        },

        // the helper functions (exported for testing)
        'getAtom': getAtom,
        'getList': getList,
        'getSExpression': getSExpression,
        'nextToken': nextToken,

        // the core public functionality
        'tokenize': tokenize,
        'stripTokens': stripTokens,
        'checkTokenSeparation': checkTokenSeparation,
        'makeSExpressions': makeSExpressions
    };

})();