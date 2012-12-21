var MaybeError = (function() {
"use strict";

    var TYPES = {
        'success': 1,
        'failure': 1,
        'error'  : 1
    };

    function ME(type, value) {
        if(!(type in TYPES)) {
            throw new Error('MaybeError constructor name');
        }
        this.type = type;
        this.value = value;
    }
    
    ME.prototype.fmap = function(f) {
        if(this.type === 'success') {
            return new ME('success', f(this.value));
        }
        return this;
    };
    
    ME.pure = function(x) {
        return new ME('success', x);
    };
    
    ME.prototype.ap = function(y) {
        if(this.type === 'success') {
            return y.fmap(this.value);
        }
        return this;
    }
    
    ME.prototype.bind = function(f) {
        if(this.type === 'success') {
            return f(this.value);
        }
        return this;
    }
    
    ME.error = function(e) {
        return new ME('error', e);
    };
    
    ME.prototype.mapError = function(f) {
        if(this.type === 'error') {
            return ME.error(f(this.value));
        }
        return this;
    };
    
    ME.prototype.plus = function(that) {
        if(this.type === 'failure') {
            return that;
        }
        return this;
    };
    
    ME.zero = new ME('failure', undefined);
    
    return ME;

})();