define([], function() {

    function Env(parent, bindings) {
        this._parent = parent;
        this._bindings = bindings;
        this._history = [];
    }


    Env.prototype.hasOwnBinding = function (name) {
        return this._bindings.hasOwnProperty(name);
    };


    Env.prototype.hasBinding = function (name) {
        if (this.hasOwnBinding(name)) {
            return true;
        }
        if (this._parent) {
            return this._parent.hasBinding(name);
        }
        return false;
    };


    Env.prototype.addBinding = function (name, value) {
        if (this.hasOwnBinding(name)) {
            throw new Error("environment already has binding for " + name);
        }
        this._bindings[name] = value;
    };


    Env.prototype.getBinding = function (name) {
        if (this.hasOwnBinding(name)) {
            return this._bindings[name];
        }
        if (this._parent) {
            return this._parent.getBinding(name);
        }
        throw new Error("could not find value for " + name);
    };
    
    
    Env.prototype.setBinding = function(name, value) {
        if (this.hasOwnBinding(name)) {
            this._bindings[name] = value;
        } else if (this._parent) {
            this._parent.setBinding(name, value);
        } else {
            throw new Error("could not find definition for " + name);
        }
    };
    
    
    Env.prototype.logEvaluation = function(sexpr) {
        this._history.push(sexpr);
        if (this._parent) {
            this._parent.logEvaluation(sexpr);
        }
    };
    
    
    Env.prototype.getLog = function() {
        return this._history;
    };


    return {
        Environment: function(parent, bindings) {
            return new Env(parent, bindings);
        }
    };

});
