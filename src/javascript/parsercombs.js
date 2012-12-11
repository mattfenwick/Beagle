var ParserCombs = (function () {

// Parse result types
    function pSuccess(rest, value) {
        return {
            'status':  'success',
            'rest'  :  rest,
            'value' :  value
        };
    }
    
    function pFail() {
        return {
            'status':  'failed'
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
            return pFail();
        }
        return pSuccess(xs.slice(1), xs[0]);
    }
    
    function satisfy(f) {
        return function(xs) {
            var t = item(xs);
            if(t.status === 'success') {
                // apply predicate to value
                if(f(t[1])) {
                    return t;
                }
            }
            return t;
        };
    }
    
    // comparison function, token, stream
    // PROBLEM:  equality comparison
    function literal(t) {
        return function(xs) {
            var r = item(xs);
            if(r.status === 'success') {
                if(r.value === t) {
                    return r;
                }
                // success -> failure
                return pFail();
            }
            // failure/error -> failure/error
            return r;
        };
    }
    
    // TODO functions below this line aren't
    //   using the real data model
    
    // predicate, parser, stream
    function check(f, p) {
        return function(xs) {
            var r = p(xs);
            if(!r) {
                return false;
            }
            if(f(r[1])) {
                return r;
            }
            return false;
        };
    }
    
    function unit(v) {
        return function(xs) {
            return [xs, v];
        };
    }
    
    function fail(xs) {
        return false;
    }
    
    function commit(message, p) {
        return function(xs) {
            var r = p(xs);
            if(r.status === 'fail') {
                return pError(message, xs);
            }
            return r;
        };
    }
    
    function either(pl, pr) {
        return function(xs) {
            var r1 = pl(xs);
            if(r1) {
                return r1;
            }
            return pr(xs);
        };
    }
    
    function bind(p, fp) {
        return function(xs) {
            var r1 = p(xs);
            if(!r1) {
                return false;
            }
            var r2 = fp(r1[1])(r1[0]);
            return r2;
        };
    }
     
    function example() {
        var p = bind(item, 
                     function(x) {
                         return literal(x);
                     });
        return p;
    }
    


    return {
        item    :  item,
        satisfy :  satisfy,
        literal :  literal,
        check   :  check,
        unit    :  unit,
        fail    :  fail,
        either  :  either,
        bind    :  bind,
        example :  example
    };

})();