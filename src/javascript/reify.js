var Reify = (function(Data) {

    var INTEGER = /^\d+$/;

    var FLOAT = /^(?:\d*\.\d+|\d+\.\d*)$/;


    function reifySymbol(sexpr) {
        if (value = sexpr.value.match(INTEGER)) {
            return Data.Number(Number(value[0]));
        }

        if (value = sexpr.value.match(FLOAT)) {
            return Data.Number(Number(value[0]));
        }

        if (sexpr.value.length > 0) {
            return Data.Symbol(sexpr.value);
        }

        // empty string is an error
        throw new Error("can't extract primitive:  symbol has empty value");
    }


    function makePrimitives(sexpr) {
        var i, value, elems;

        if (sexpr.type === 'list') {
            elems = [];

            for (i = 0; i < sexpr.value.length; i++) {
                elems.push(makePrimitives(sexpr.value[i]));
            }

            return Data.List(elems);
        }

        if (sexpr.type === "string") {
            return Data.String(sexpr.value);
        }

        if (sexpr.type === 'symbol') {
            return reifySymbol(sexpr);
        }

        throw new Error("unrecognized s-expression type: " + sexpr.type);
    }


    return {
        makePrimitives: makePrimitives
    };

})(Data);