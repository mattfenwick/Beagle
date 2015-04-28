'use strict';


function Env(parent, bindings) {
    this.parent = parent;
    this.bindings = bindings;
}

Env.prototype.hasOwnBinding = function (name) {
    return this.bindings.hasOwnProperty(name);
};

Env.prototype.hasBinding = function (name) {
    if (this.hasOwnBinding(name)) {
        return true;
    }
    if (this.parent) {
        return this.parent.hasBinding(name);
    }
    return false;
};

Env.prototype.addBinding = function (name, value) {
    if (this.hasOwnBinding(name)) {
        throw new Error("environment already has binding for " + name);
    }
    this.bindings[name] = value;
};

Env.prototype.getBinding = function (name) {
    if (this.hasOwnBinding(name)) {
        return this.bindings[name];
    }
    if (this.parent) {
        return this.parent.getBinding(name);
    }
    throw new Error("could not find value for " + name);
};

Env.prototype.setBinding = function(name, value) {
    if (this.hasOwnBinding(name)) {
        this.bindings[name] = value;
    } else if (this.parent) {
        this.parent.setBinding(name, value);
    } else {
        throw new Error("could not find definition for " + name);
    }
};

module.exports = {
    'Environment': Env
};

