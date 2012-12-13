var ParserCombs = (function () {
"use strict";

// Parse result types
    function pSuccess(rest, value) {
        return {
            'status':  'success',
            'rest'  :  rest,
            'value' :  value
        };
    }
    
    function pFail(rest) {
        return {
            'status':  'failed',
            'rest'  :  rest
        };
    }
    
    function pError(message, rest) {
        return {
            'status'  :  'error',
            'rest'    :  rest,
            'message' :  message
        };
    }


    function item(xs) {
        if(xs.length === 0) {
            return pFail(xs);
        }
        return pSuccess(xs.slice(1), xs[0]);
    }
    
    function equality(l, r) {
        return l === r;
    }
    
    // PROBLEM:  equality comparison
    // t -> Parser t t
    function literal(t, f) {
        var eq = f ? f : equality;
        return function(xs) {
            var r = item(xs);
            if (r.status === 'success') {
                if (eq(r.value, t)) {
                    return r;
                }
                // success -> failure
                return pFail(xs);
            }
            // failure/error -> failure/error
            return r;
        };
    }
    
    // (a -> Bool) -> Parser t a -> Parser t a
    function check(f, p) {
        return function(xs) {
            var r = p(xs);
            if(r.status === 'success') {
                if(f(r.value)) {
                    return r;
                }
                return pFail(xs);
            }
            return r;
        };
    }
    
    function satisfy(f) {
        return check(f, item);
    }
    
    function unit(v) {
        return function(xs) {
            return pSuccess(xs, v);
        };
    }
    
    function fail(xs) {
        return pFail(xs);
    }
    
    function commit(p, message) {
        return function(xs) {
            var r = p(xs);
            if(r.status === 'failed') {
                return pError(message, xs);
            }
            return r;
        };
    }
    
    function either(pl, pr) {
        return function(xs) {
            var r1 = pl(xs);
            if(r1.status === 'failed') {
                return pr(xs);
            }
            // success/error in the first parser 
            //   are left alone
            return r1;
        };
    }
    
    // Parser t a -> (a -> Parser t b) -> Parser t b
    function bind(p, fp) {
        return function(xs) {
            var r1 = p(xs);
            if(r1.status === 'success') {
                var r2 = fp(r1.value)(r1.rest); // hmm ... f(a)(b) or f(a, b) ???
                return r2;
            }
            return r1;
        };
    }
    
    // [Parser t a] -> Parser t [a]
    function all(ps) {
        return function(xs) {
            var r, i,
                tokens = xs,
                vals = [];
            for(i = 0; i < ps.length; i++) {
                r = ps[i](tokens);
                if (r.status === 'success') {
                    tokens = r.rest;
                    vals.push(r.value);
                } else {
                    return r;
                }
            }
            return pSuccess(tokens, vals);
        };
    }
    
    // [t] -> Parser t [t]
    function string(s) {
        return function(xs) {
            var i;
            for(i = 0; i < s.length; i++) {
                if (s[i] !== xs[i]) {
                    return pFail(xs.slice(i));
                }
            }
            return pSuccess(xs.slice(i), s);
        };
    }
    
    // Parser t a -> Parser t [a]
    function many0(p) {
        return function(xs) {
             var r,
                tokens = xs,
                vals   = [];
            while(true) {
                r = p(tokens);
                if (r.status === 'success') {
                    tokens = r.rest;
                    vals.push(r.value);
                } else if (r.status === 'error') {
                    return r;
                } else {
                    return pSuccess(tokens, vals);
                }
            }
        };
    }
    
    function many1(p) {
        return function(xs) {
            var r = many0(p)(xs);
            if (r.status === 'error') {
                return r;
            }
            // many0 always returns an error or
            //   a success -- never a failure
            if (r.value.length > 0) {
                return r;
            }
            return pFail(xs);
        };
    }
    
    // (a -> b) -> Parser t a -> Parser t b
    function fmap(f, p) {
        return function(xs) {
            var r = p(xs);
            if(r.status === 'success') {
                return pSuccess(r.rest, f(r.value));
            }
            return r;
        };
    }
    
    function any(ps) {
        return function(xs) {
            var r = pFail(xs),
                i;
            for(i = 0; i < ps.length; i++) {
                r = ps[i](xs);
                if(r.status === 'success' || r.status === 'error') {
                    return r;
                }
            }
            return r;
        };
    }
    
    function seq2L(pl, pr) {
        return fmap(function (x) {return x[0];}, all([pl, pr]));
    }
    
    function seq2R(pl, pr) {
        return fmap(function (x) {return x[1];}, all([pl, pr]));
    }


    return {
        item    :  item,
        satisfy :  satisfy,
        literal :  literal,
        check   :  check,
        unit    :  unit,
        fail    :  fail,
        commit  :  commit,
        either  :  either,
        bind    :  bind,
        all     :  all,
        string  :  string,
        'many0' :  many0,
        'many1' :  many1,
        fmap    :  fmap,
        any     :  any,
        seq2L   :  seq2L,
        seq2R   :  seq2R,
        
        pFail   :  pFail,
        pSuccess:  pSuccess,
        pError  :  pError
    };

})();