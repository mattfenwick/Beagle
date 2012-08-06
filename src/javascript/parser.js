var Parser = (function() {
    
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
    
    
    function ASTNumber(value) {
        this.asttype = 'number';
        this.value = Number(value);
    }
    
    function Symbol(value) {
        if(typeof(value) !== 'string') {
            throw new ParseError("Symbol needs a string", value);
        };
        this.asttype = 'symbol';
        this.value = value;
    }
    
    function ASTChar(value) {
        if(typeof(value) !== 'string') {
            throw new ParseError("Symbol needs a string", value);
        };
        this.asttype = 'char';
        this.value = value;
    }
    
    function ASTList(elems) {
        if(elems.length === undefined) {
            throw new ParseError("ASTList needs an array", elems);
        }
        this.asttype = 'list';
        this.elements = elems;
    }
    
    function Application(op, args) {
        if(!op) {
            throw new ParseError("an application needs an operator (got nothing)", [op, args]);
        }
        if(args.length === undefined) {
            throw new ParseError("Application needs an array of arguments (2nd arg)", args);
        }
        this.asttype = 'application';
        this.operator = op;
        this.arguments = args;
    }
    
    
/////////////////////// functions
    
    // String -> ASTList ASTChar
    function expandString(str) {
        // this assumes that 'abcd'.split('') returns
        //   an array of length 4
        var chars = str.split('').map(function (x) {
            return new ASTChar(x);
        });
        
        return new ASTList(chars);
    }
    

    var TOKEN_OPS = {
        'integer' : function(x) {return new ASTNumber(x);},
        'float'   : function(x) {return new ASTNumber(x);},
        'string'  : function(x) {return expandString(x);},
        'symbol'  : function(x) {return new Symbol(x);}
    };

    // [Token] -> Maybe (ASTNode, [Token])
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


    // tokentype -> tokentype -> [Token] -> ([ASTNode] -> ASTNode) -> Maybe ASTNode
    //   returns false if tokens is empty or doesn't start with token of type start
    //   throws an error if first token is of type stop
    //   throws an error if a properly 'balanced' object can't be found
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
                result: success(elems),
                rest: tokens.slice(1)
            };
        }

        // uh-oh!  we didn't find a stop token
        throw new ParseError(start + " token found without matching " + stop, inputTokens);
    }
    
    
    function getApplication(tokens) {
        function callback(objs) {
            return new Application(objs[0], objs.slice(1));
        }
        return getDelimited('open-paren', 'close-paren', tokens, callback);
    }
    
    
    function getList(tokens) {
        function callback(objs) {
            return new ASTList(objs);
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
    // [Token] -> Maybe [AST]
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
        // data
        'ParseError': function (m, v) {
            return new ParseError(m, v);
        },
        'ASTNumber': function(x) {
            return new ASTNumber(x);
        },
        'ASTChar': function(x) {
            return new ASTChar(x);
        },
        'ASTList': function(xs) {
            return new ASTList(xs);
        },
        'Symbol': function(x) {
            return new Symbol(x);
        },
        'Application': function(op, args) {
            return new Application(op, args);
        },
        
        // helper functions
        'expandString'   : expandString,
        'getAtom'        : getAtom,
        'getApplication' : getApplication,
        'getList'        : getList,
        'getNextForm'    : getNextForm,

        // the public function
        'makeAST'        : makeAST,        
    };

})();