var Parser = (function(Data) {
    
    
    function ParseError(message, value) {
        this.type = 'ParseError';
        this.message = message;
        this.value = value;
    }
    
    
    ParseError.prototype = new Error();
    ParseError.prototype.constructor = ParseError;


    ParseError.prototype.toString = function() {
        return this.type + " during AST construction: " + this.message;
    };
    
    
/////////////////////// new stuff

    var TOKEN_OPS = {
        'integer' : function(str) {return Data.Number(Number(str));},
        'float'   : function(str) {return Data.Number(Number(str));},
        'string'  : Data.makeCharList,
        'symbol'  : Data.Symbol
    };

    // [Token] -> Maybe (LispObject, [Token])
    //   returns false if token stream is empty or first token is NOT a symbol/string/integer/float
    function getAtom(tokens) {
        if (tokens.length === 0) {
            return false;
        }

        var first = tokens[0],
            op = TOKEN_OPS[first.type];

        if (op) {
            return {
                result: op(first.value),
                rest: tokens.slice(1)
            };
        }

        return false;
    }


    // tokentype -> tokentype -> [Token] -> ([LispObject] -> LispObject) -> Maybe LispObject
    //   returns false if tokens is empty or doesn't start with token of type start
    //   throws an error if first token is of type stop
    //   throws an error if a properly 'balanced' LispObject can't be found
    function getDelimited(start, stop, tokens, success) {
        var sexpr, elems = [],
            inputTokens = tokens;

        if (tokens.length === 0) {
            return false;
        }

        if (tokens[0].type === stop) {
            throw new ParseError(stop + " token found without matching " + start, inputTokens);
        }

        // must begin with a start token
        if (tokens[0].type !== start) {
            return false;
        }

        tokens = tokens.slice(1);

        // keep going until a stop token
        while (tokens[0] && (tokens[0].type !== stop)) {
            // could be arbitrarily many nested forms
            sexpr = getNextForm(tokens);
            if (!sexpr) {
                return false;
            }
            elems.push(sexpr.result);
            tokens = sexpr.rest;
        }

        // must end with a stop token
        if (tokens[0] && tokens[0].type === stop) {
            return {
                result: success(elems), // Data.Application(elems[0], elems.slice(1)), // what if 'elems' is empty?
                rest: tokens.slice(1)
            };
        }

        // uh-oh!  we didn't find a stop token
        throw new ParseError(start + " token found without matching " + stop, inputTokens);
    }
    
    
    function getApplication(tokens) {
        function callback(objs) {
            return Data.Application(objs[0], objs.slice(1));
        }
        return getDelimited('open-paren', 'close-paren', tokens, callback);
    }
    
    
    function getList(tokens) {
        function callback(objs) {
            return Data.List(objs);
        }
        return getDelimited('open-square', 'close-square', tokens, callback);
    }


    function getNextForm(tokens) {
        var sexpr;

        if (tokens.length === 0) {
            return false;
        }

        // an s-expression is either an atom
        sexpr = getAtom(tokens);
        if (sexpr) {
            return sexpr;
        }
        
        // an application
        sexpr = getApplication(tokens);
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


    // assumes the tokens are of type 'string', 'symbol', 'integer', 'float',
    //   'open-paren', 'close-paren', 'open-square', and 'close-square'
    //   anything else should throw an exception
    // [Token] -> Maybe [LispObject]
    function makeAST(tokens) {
        var sexprs = [],
            sexpr;

        while (sexpr = getNextForm(tokens)) {
            sexprs.push(sexpr.result);
            tokens = sexpr.rest;
        }

        if (tokens.length !== 0) {
            throw new ParseError('unconsumed tokens remaining after parse', tokens);
        }

        return sexprs;
    }    
    


    return {
        'ParseError': function (m, v) {
            return new ParseError(m, v);
        },
        
        // helper functions
        'getAtom'        : getAtom,
        'getApplication' : getApplication,
        'getList'        : getList,
        'getNextForm'    : getNextForm,

        // the public function
        'makeAST'        : makeAST,        
    };

})(Data);