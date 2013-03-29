define(["app/beagle", function(Beagle) {

    function isString(obj) {
        if(obj.type !== 'list') {
            return false;
        }
        return obj.value.every(function(c) {return c.type === 'char';});
    }
 

    function displayEnv(envi) {
        var tab = $("#env"),
            q, val;
        $("#env .envrow").remove();
        for(q in envi._bindings) {
            val = envi.getBinding(q);
            tab.append('<tr class="envrow"><td>' + q + "</td><td>" 
                + val.type + "</td><td>" + _.escape(printer(val)) + "</td></tr>");
        }
    }


  $(document).ready(function() {

    var evaler = new Repl.Evaluator(),
        env = evaler.getEnvironment();

    //$("#runner").click(function(e) { //
    $("#expr").bind('keydown', function(e) {
        var code = (e.keyCode ? e.keyCode : e.which);
        if( code == 13 ) {
            var str = $("#expr").val();
            var result = Beagle.exec(str, Beagle.environment);
            if ( result.status === 'success' ) {
            
            } else { // it's an error
            
            }
            return false;
        }
        return true;
    });

    evaler.bind("success", function(obj) {
        var h = $("#history");
        h.append("<li><pre>" + _.escape("> " + obj.input) + "</pre></li>");
        // remember -- only allowing one form (for now) so just take the first
        obj.results.map(function(x) {
            h.append("<li>" + _.escape(JSON.stringify(printer(x))) + "</li>");
        });
    });
  
    evaler.bind("success", function(obj) {
        displayEnv(env);
        $("#lastsexpr").text(JSON.stringify(obj.tokens));
        $("#lastprims").text(JSON.stringify(obj.asts) + "<br />" + JSON.stringify(obj.results[0]));
        $("#expr").val(""); // clear the input area
    });

    evaler.bind("error", function(obj) {
        var h = $("#history"),
            sed = JSON.stringify(obj);
        h.append("<li><pre>" + _.escape("> " + obj.input) + "</pre></li>");
        h.append("<li>" + "ERROR: " + _.escape(sed) + "</li>"); // TODO need to stabilize up error interface to provide better messages
    });
  
    evaler.bind('all', function() {
        var h = $("#history");
        h.scrollTop(h.prop("scrollHeight"));
    });

        // get the page set-up:  show the environment
        displayEnv(env);

    });

});