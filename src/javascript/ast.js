var AST = (function() {


    
    function ParseError(errortype, message, value) {
        this.type = 'ParseError';
        this.errortype = errortype;
        this.message = message;
        this.value = value;
    }
    
    
    ParseError.prototype = new Error();
    ParseError.prototype.constructor = ParseError;


    ParseError.prototype.toString = function() {
        return this.type + " during AST construction: " + this.message + JSON.stringify(this.value);
    };
    
    
    function ASTNumber(value) {
        if(typeof(value) !== 'string') {
            throw new ParseError("TypeError", "ASTNumber needs a string", value);
        };
        this.asttype = 'number';
        this.value = Number(value);
    }
    
    function Symbol(value) {
        if(typeof(value) !== 'string') {
            throw new ParseError("TypeError", "Symbol needs a string", value);
        };
        this.asttype = 'symbol';
        this.value = value;
    }
    
    function ASTChar(value) {
        if(typeof(value) !== 'string') {
            throw new ParseError("TypeError", "ASTChar needs a string", value);
        };
        this.asttype = 'char';
        this.value = value;
    }
    
    function ASTList(elems) {
        if(elems.length === undefined) {
            throw new ParseError("TypeError", "ASTList needs an array", elems);
        }
        elems.map(function(arg, ix) {
            if(!isExpression(arg)) {
                throw new ParseError("TypeError", "all elements in a list must be expressions", [arg, ix + 1]);
            }
        });
        this.asttype = 'list';
        this.elements = elems;
    }

    function isExpression(astnode) {
        var EXPRESSIONS = {
            'symbol'     :  1,
            'number'     :  1,
            'char'       :  1,
            'list'       :  1,
            'application':  1,
            'cond'       :  1,
            'lambda'     :  1
        };
        
        return EXPRESSIONS[astnode.asttype];
    }
    
    function Application(args) {
        var opTypes = {'application': 1, 'cond': 1, 
                'lambda': 1, 'symbol': 1};
        if(!args[0]) {
            throw new ParseError("ValueError", "application needs operator", args);            
        } else if(!opTypes[args[0].asttype]) {
            throw new ParseError("TypeError", "application needs app/cond/lambda/symbol for operator", args);
        }
        args.slice(1).map(function(arg, ix) {
            if(!isExpression(arg)) {
                throw new ParseError("TypeError", "all arguments to application must be expressions", [arg, ix + 1]);
            }
        });
        this.asttype = 'application';
        this.operator = args[0];
        this.arguments = args.slice(1);
    }

    function Define(args) {
        if(args.length !== 2) {
            throw new ParseError("NumArgsError", "'define' needs two args", args);
        }
        var symbol = args[0],
            astnode = args[1];

        if(symbol.asttype !== 'symbol') {
            throw new ParseError("TypeError", "'define' needs a symbol as 1st argument", symbol);
        }

        if(!isExpression(astnode)) {
            throw new ParseError("TypeError", "2nd arg to 'define' must be an expression", astnode);
        }

        this.asttype = 'define';
        this.symbol = symbol;
        this.astnode = astnode;
    }

    function SetBang(args) {
        if(args.length !== 2) {
            throw new ParseError("NumArgsError", "'set!' needs two args", args);
        }
        var symbol = args[0],
            astnode = args[1];

        if(symbol.asttype !== 'symbol') {
            throw new ParseError("TypeError", "'set!' needs a symbol as 1st argument", symbol);
        }

        if(!isExpression(astnode)) {
            throw new ParseError("TypeError", "2nd arg to 'set!' must be an expression", astnode);
        }

        this.asttype = 'set!';
        this.symbol = symbol;
        this.astnode = astnode;
    }

    function Cond(args) {
        if(args.length !== 2) {
            throw new ParseError('NumArgsError', "'cond' needs two args " + args.length, args);
        }
        var pairs = args[0],
            elseValue = args[1];
        if(pairs.asttype !== 'list') {
            throw new ParseError('TypeError', '1st arg to cond must be a list', args);
        }
        pairs.elements.map(function(p, ix) {
            if(p.asttype !== 'list') {
                throw new ParseError("TypeError", "in 1st arg: 'cond' needs lists", ix + 1);
            } else if(p.elements.length !== 2) {
                throw new ParseError("ValueError", "'cond' lists must be of length 2", ix + 1);
            }
        });
        if(!isExpression(elseValue)) {
            throw new ParseError("TypeError", "cond else-value must be an expression", elseValue);
        }
        this.asttype = 'cond';
        this.pairs = pairs;
        this.elseValue = elseValue;
    }

    function Lambda(args) {
        if(args.length < 2) {
            throw new ParseError("NumArgsError", "'lambda' needs at least two args", args);
        }
        var params = args[0],
            bodies = args.slice(1),
            names = {};
        if(params.asttype !== 'list') {
            throw new ParseError("TypeError", 'lambda needs a list as 1st arg', params.asttype);
        }
        params.elements.map(function(s, ix) {
            if(s.asttype !== 'symbol') {
                throw new ParseError("TypeError", 'lambda needs symbols in its parameter list', ix + 1);
            }
            if(s.value in names) {
                throw new ParseError("ValueError", "duplicate parameter name in 'lambda'", args);
            }
            names[s.value] = 1;
        });
        this.lastForm = bodies.pop();
        if(!isExpression(this.lastForm)) {
            throw new ParseError("TypeError", "last 'lambda' body form must be an expression", args);
        }
        this.asttype = 'lambda';
        this.parameters = params;
        this.bodyForms = bodies;
    }
    
    ///////
    
        // String -> ASTList ASTChar
    function expandString(str) {
        // this assumes that 'abcd'.split('') returns
        //   an array of length 4
        var chars = str.split('').map(function (x) {
            return new ASTChar(x);
        });
        
        return new ASTList(chars);
    }
    
    function makeAST() {
        throw new Error("unimplemented");
    }
    
    ///////// 
    


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
        'Define': function(args) {
            return new Define(args);
        },
        'SetBang': function(args) {
            return new SetBang(args);
        },
        'Lambda': function(args) {
            return new Lambda(args);
        },
        'Cond': function(args) {
            return new Cond(args);
        },
        
        // helper functions
        'expandString'   : expandString,

        // the public function
        'makeAST'        : makeAST
    };

})();