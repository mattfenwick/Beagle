
require(["repl/model", "repl/controller", "app/evaluate"], function(Model, Cont, evaluate) {
    
    $(document).ready(function() {
        new Cont(new Model(evaluate.getDefaultEnv()));
    });
    
});
