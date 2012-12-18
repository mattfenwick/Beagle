var ParserCombs = (function () {
"use strict";

    var TYPES = {
        'success': 1,
        'failure': 1,
        'error'  : 1
    };

    function Result(type, value) {
        if(!(type in TYPES)) {
            throw new Error('bad Result type ' + type);
        }
        this.type = type;
        this.value = value;
    }
    
    Result.prototype.fmap = function(f) {
        if(this.type === 'success') {
            return new Result('success', f(this.value));
        }
        return this;
    };
    
    Result.pure = function(x) {
        return new Result('success', x);
    };
    
    Result.prototype.ap = function(y) {
        if(this.type === 'success') {
            return y.fmap(this.value);
        }
        return this;
    }
    
    Result.prototype.bind = function(f) {
        if(this.type === 'success') {
            return f(this.value);
        }
        return this;
    }
    
    Result.error = function(e) {
        return new Result('error', e);
    };
    
    Result.prototype.plus = function(that) {
        if(this.type === 'failure') {
            return that;
        }
        return this;
    };
    
    Result.zero = new Result('failure', undefined);
    
    Result.prototype.mapError = function(f) {
        if(this.type === 'error') {
            return Result.error(f(this.value));
        }
        return this;
    };
    
    
    // ([t] -> m ([t], a)) -> Parser m t a
    function Parser(f) {
        this.parse = f;
    }
    
    // (a -> b) -> Parser t a -> Parser t b
    Parser.prototype.fmap = function(f) {
        var self = this;
        return new Parser(function(xs) {
            return self.parse(xs).fmap(function(r) {
                return {
                    rest: r.rest,
                    result: f(r.result)
                };
            });
        });
    };
    
    // a -> Parser t a
    Parser.pure = function(x) {
        return new Parser(function(xs) {
            return Result.pure({rest: xs, result: x});
        });
    };
    
    // skipping Applicative ... for now
    
    // m a -> (a -> m b) -> m b
    // ([t] -> m ([t], a)) -> (a -> [t] -> m ([t], b)) -> [t] -> m ([t], b)
    Parser.prototype.bind = function(f) {
        var self = this;
        return new Parser(function(xs) {
            var r = self.parse(xs);
            if(r.type === 'success') {
                return f(r.value.result).parse(r.value.rest);
            }
            return r;
        });
    };
    
    Parser.prototype.plus = function(that) {
        var self = this;
        return new Parser(function(xs) {
            return self.parse(xs).plus(that.parse(xs));
        });
    };
    
    Parser.zero = new Parser(function(xs) {
        return Result.zero;
    });
    
    // Parser [t] t a
    Parser.error = function(value) {
        return new Parser(function(xs) {
            return Result.error(value);
        });
    };
    
    // (e -> m) -> Parser e t a -> Parser m t a
    Parser.prototype.mapError = function(f) {
        var self = this;
        return new Parser(function(xs) {
            return self.parse(xs).mapError(f);
        });
    };
    
    // Parser t [t]
    Parser.get = new Parser(function(xs) {
        return Result.pure({rest: xs, result: xs});
    });
    
    // [t] -> Parser t ()   // just for completeness
    Parser.put = function(xs) {
        return new Parser(function() {
            return Result.pure({rest: xs, result: null});
        });
    };

    // parsers
    
    // Parser t t
    Parser.item = new Parser(function(xs) {
        if(xs.length === 0) {
            return Result.zero;
        }
        var x = xs[0];
        return Result.pure({rest: xs.slice(1), result: x});
    });
    
    // (a -> Bool) -> Parser t a -> Parser t a
    Parser.prototype.check = function(p) {
        var self = this;
        return new Parser(function(xs) {
            var r = self.parse(xs);
            if(r.type !== 'success') {
                return r;
            } else if(p(r.value.result)) {
                return r;
            }
            return Result.zero;
        });
    };
    
    function equality(x, y) {
        return x === y;
    }

    // t -> Maybe (t -> t -> Bool) -> Parser t t    
    Parser.literal = function(x, f) {
        var eq = f ? f : equality;
        return Parser.item.check(function (y) {
                                     return eq(x, y);
                                 });
    };
    
    // (t -> Bool) -> Parser t t
    Parser.satisfy = function(pred) {
        return Parser.item.check(pred);
    };
    
    // Parser t a -> Parser t [a]
    Parser.prototype.many0 = function() {
        var self = this;
        return new Parser(function(xs) {
            var vals = [],
                tokens = xs,
                r;
            while(true) {
                r = self.parse(tokens);
                if(r.type === 'success') {
                    vals.push(r.value.result);
                    tokens = r.value.rest;
                } else if(r.type === 'failure') {
                    return Result.pure({rest: tokens, result: vals});
                } else { // must respect errors
                    return r;
                }
            }
        });
    };
    
    // Parser t a -> Parser t [a]
    Parser.prototype.many1 = function() {
        return this.many0().check(function(x) {return x.length > 0;});
    };

    // (a -> b -> ... z) -> (Parser t a, Parser t b, ...) -> Parser t z
    // example:   app(myFunction, parser1, parser2, parser3, parser4)
    Parser.app = function(f, ps__) {
        var p = Parser.all(Array.prototype.slice.call(arguments, 1));
        return p.fmap(function(rs) {
            return f.apply(undefined, rs); // 'undefined' gets bound to 'this' inside f
        });
    };
    
    // a -> Parser t a -> Parser t a
    Parser.prototype.optional = function(x) {
        return this.plus(Parser.pure(x));
    };
    
    // [Parser t a] -> Parser t [a]
    Parser.all = function(ps) {
        return new Parser(function(xs) {
            var vals = [],
                i, r,
                tokens = xs;
            for(i = 0; i < ps.length; i++) {
                r = ps[i].parse(tokens);
                if(r.type === 'error') {
                    return r;
                } else if(r.type === 'success') {
                    vals.push(r.value.result);
                    tokens = r.value.rest;
                } else {
                    return Result.zero;
                }
            }
            return Result.pure({rest: tokens, result: vals});
        });
    };
    
    // Parser t a -> Parser t ()
    Parser.prototype.not0 = function() {
        var self = this;
        return new Parser(function(xs) {
            var r = self.parse(xs);
            if(r.type === 'error') {
                return r;
            } else if(r.type === 'success') {
                return Result.zero;
            } else {
                return Result.pure({rest: xs, result: null}); // or undefined?  ???
            }
        });
    };
    
    // Parser t a -> Parser t t
    Parser.prototype.not1 = function() {
        return this.not0().seq2R(Parser.item);
    };
    
    // e -> Parser e t a
    Parser.prototype.commit = function(e) {
        return this.plus(Parser.error(e));
    };
    
    Parser.prototype.seq2L = function(p) {
        return Parser.all([this, p]).fmap(function(x) {return x[0];});
    };
    
    Parser.prototype.seq2R = function(p) {
        return Parser.all([this, p]).fmap(function(x) {return x[1];});
    };
    
    // purpose:  '[].map' passes in index also
    //   which messed up literal because it
    //   expects 2nd arg to be a function or undefined
    // this function ensures that doesn't happen
    function safeMap(array, f) {
        var out = [], i;
        for(i = 0; i < array.length; i++) {
            out.push(f(array[i]));
        }
        return out;
    }
    
    // [t] -> Parser t [t]
    // n.b.:  [t] != string !!!
    Parser.string = function(str) {
        return Parser.all(safeMap(str, Parser.literal)).seq2R(Parser.pure(str));
    };

    // [Parser t a] -> Parser t a
    Parser.any = function (ps) {
        return new Parser(function(xs) {
            var r = Result.zero,
                i;
            for(i = 0; i < ps.length; i++) {
                r = ps[i].parse(xs);
                if(r.type === 'success' || r.type === 'error') {
                    return r;
                }
            }
            return r;
        });
    };


    return {
        Result:  Result,
        Parser:  Parser
    };

})();