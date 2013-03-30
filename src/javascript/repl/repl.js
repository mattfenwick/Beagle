define(["app/beagle", "repl/formatter"], function(Beagle, Formatter) {
    "use strict";

    function displayEnv(envi) {
        var tab = $("#env"),
            q, val;
        $("#env .envrow").remove();
        for(q in envi._bindings) {
            val = envi.getBinding(q);
            console.log('binding: ' + JSON.stringify(val));
            tab.append('<tr class="envrow"><td>' + q + "</td><td>" + val.type + 
                "</td><td>" + Formatter.format(val) + "</td></tr>");
        }
    }
    
    function print(o) {
        return JSON.stringify(Formatter.format(o));
    }

    function pushSuccess(obj) {
        var h = $("#history");
        // strategy:  zip the asts and results (or something),
        //   then print each pair out together
        for(var i = 0; i < obj.asts.length; i++) {
            // print the ast
            h.append("<li><pre>" + "> " + print(obj.asts[i]) + "</pre></li>");
            // print the result
            h.append("<li>" + print(obj.results[i]) + "</li>");
        }
    }
  
    function clearInputArea() {
        $("#expr").val("");
    }
  
    function setScroll() {
        var h = $("#history");
        h.scrollTop(h.prop("scrollHeight"));
    }
    
    function onSuccess(result) {
        pushSuccess(result);
        clearInputArea();
        displayEnv(Beagle.environment);
        setScroll();
    }

    function pushError(obj) {
        var h = $("#history"),
            sed = JSON.stringify(obj);
        h.append("<li><pre>" + "> " + obj.input + "</pre></li>");
        h.append("<li>" + "ERROR: " + sed + "</li>");
    }
    
    function onError(result) {
        pushError(result);
        setScroll();
    }

    function setUp() {
        $(document).ready(function() {
    
            //$("#runner").click(function(e) { //
            $("#expr").bind('keydown', function(e) {
                var code = (e.keyCode ? e.keyCode : e.which);
                if( code == 13 ) { // why isn't this triple-equal?
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
            
            displayEnv(Beagle.environment);
        });
    }
    
    return {
        'setUp': setUp
    };

});