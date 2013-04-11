define(function() {

    function Env(elem) {
        this.elem = elem;
    }
    
    Env.prototype.display = function(environment) {
        var q, val;
        $("#env .envrow").remove(); // how can I get rid of this to remove the `$` dependency?
        for(q in environment._bindings) {
            val = environment.getBinding(q);
            this.elem.append(['<tr class="envrow"><td>'
                              , q
                              , "</td><td>"
                              , val.type
                              , "</td><td>"
                              , JSON.stringify(val)
                              , "</td></tr>"
                             ].join(''));
        }
    };
    
    return Env;

});