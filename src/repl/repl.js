
var Repl = (function(_, Backbone, Beagle) {
"use strict";


function Evaluator() {
  this._history = [];
}


Evaluator.prototype.evalString = function(str) {
  var evaled, obj;
  try {
    evaled = Beagle.lisp(str);
    obj = {
      'expression': str,
      'result': evaled
    };
    this._history.push(obj);
    this.trigger("success", obj);
  } catch(e) {
    obj = {
      'expression': str,
      'errormessage': e
    };
    this._history.push(obj);
    this.trigger("error", obj);
  };
}


_.extend(Evaluator.prototype, Backbone.Events);


return {
  'Evaluator': function() {return new Evaluator();}
};

})(_, Backbone, Beagle);
