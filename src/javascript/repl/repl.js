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
        return Formatter.format(o);
    }

    function pushSuccess(input, obj) {
        var h = $("#history");
        h.append("<li><pre>" + "> " + input + "</pre></li>");
        
        for(var i = 0; i < obj.asts.length; i++) {
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
    
    function onSuccess(input, result) {
        pushSuccess(input, result);
        clearInputArea();
        displayEnv(Beagle.environment);
        setScroll();
    }

    function pushError(input, obj) {
        var h = $("#history"),
            sed = JSON.stringify(obj);
        h.append("<li><pre>" + "> " + input + "</pre></li>");
        h.append("<li>" + "ERROR: " + sed + "</li>");
    }
    
    function onError(input, result) {
        pushError(input, result);
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
                        onSuccess(str, result.value);
                    } else { // it's an error
                        onError(str, result.value);
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