'use strict';

require.config({
    paths: {
        'jquery'    : '../bower_components/jquery/jquery',
        'HTML'      : '../bower_components/HTML/HTML',
        'unparse-js': '../bower_components/unparse-js/'
    }
});

require(["repl/model", "repl/controller", "app/evaluate"], function(Model, Cont, evaluate) {
    
    $(document).ready(function() {
        new Cont(new Model(evaluate.getDefaultEnv()));
    });
    
});
