var Repl = (function(_, Backbone, Beagle) {
    "use strict";

    function Evaluator() {
        this._history = [];
        this._lisp = Beagle.exec;
        this._environment = Beagle.environment;
    }

    Evaluator.prototype.evalString = function(str) {
        var evaled = this._lisp(str);
        this._history.push(evaled);
        if(evaled.status === 'success') {
            this.trigger("success", evaled);
        } else {
            this.trigger("error", evaled);
        }
    }

    Evaluator.prototype.getEnvironment = function() {
        return this._environment;
    }

    _.extend(Evaluator.prototype, Backbone.Events);

    return {
        'Evaluator': function() {return new Evaluator();}
    };

})(_, Backbone, Beagle);
