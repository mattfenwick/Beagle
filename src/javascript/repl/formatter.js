define(function() {
    "use strict";

    function isString(obj) {
        if(obj.type !== 'list') {
            return false;
        }
        return obj.value.every(function(c) {return c.type === 'char';});
    }

    function format(obj) {
        if(isString(obj)) {
            return '"' + obj.value.map(function(c) {return c.value;}).join('') + '"';
        }
      
        if( obj.type === 'function' ) {
            return '<function>';
        }
      
        if( obj.type === 'null' ) {
            return '<null>';
        }
      
        if( obj.type === 'list' ) {
            return '[' + obj.value.map(printer).join(', ') + ']';
        }
      
        if( obj.type === 'char' ) {
            return "'" + obj.value + "'";
        }
      
        if( obj.type === 'boolean' ) {
            return obj.value;
        }
      
        if( obj.type === 'number' ) {
            return obj.value;
        }
    
        console.log('unrecognized object type in format: ' + obj.type);
      
        return obj;
    }

    return {
        'format': format
    };
});
