define(['views/input', 'views/history', 'views/env'], function(I, H, E) {

    function Controller(model) {
        this.model = model;
        this.history = new H($("#history"));
        this.env = new E($("#env"));
        this.input = new I($("#expr"), this);
    }
    
    Controller.prototype.evalString = function(str) {
        var result = this.model.evalString(str);
        this.history.add(str, result);
        this.input.reset(result);
        this.env.display(this.model.environment);
    };
    
    return Controller;
    
});
