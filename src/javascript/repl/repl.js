define(["app/beagle", "libs/underscore-amd", "repl/formatter", function(Beagle, _, Formatter) {
    "use strict";

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

    function pushSuccess(obj) {
        var h = $("#history");
        // strategy:  zip the asts and results (or something),
        //   then print each pair out together
        for(var i = 0; i < obj.asts.length; i++) {
            // print the ast
            h.append("<li><pre>" + _.escape("> " + obj.input) + "</pre></li>");
            // print the result
            h.append("<li>" + _.escape(JSON.stringify(Formatter.format(x))) + "</li>");
        }
    }
  
    function clearInputArea(obj) {
        $("#expr").val("");
    };
  
    function setScroll() {
        var h = $("#history");
        h.scrollTop(h.prop("scrollHeight"));
    };
    
    function onSuccess(result) {
        pushSuccess(result.value);
        clearInputArea();
        displayEnv(Beagle.environment);
        setScroll();
    }

    function pushError(obj) {
        var h = $("#history"),
            sed = JSON.stringify(obj);
        h.append("<li><pre>" + _.escape("> " + obj.input) + "</pre></li>");
        h.append("<li>" + "ERROR: " + _.escape(sed) + "</li>"); // TODO need to stabilize up error interface to provide better messages
    };
    
    function onError(result) {
        pushError(result);
        setScroll();
    }

    function setUp() {
        $(document).ready(function() {
    
            //$("#runner").click(function(e) { //
            $("#expr").bind('keydown', function(e) {
                var code = (e.keyCode ? e.keyCode : e.which);
                if( code == 13 ) {
                    var str = $("#expr").val();
                    var result = Beagle.exec(str, Beagle.environment);
                    if ( result.status === 'success' ) {
                        onSuccess(result.value);
                    } else { // it's an error
                        onError(result.value);
                    }
                    return false;
                }
                return true;
            });
        });
    }

});