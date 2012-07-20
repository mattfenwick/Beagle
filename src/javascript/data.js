var Data = (function () {
    "use strict";

    function MyNumber(value) {
        this.value = value;
        this.type = 'number';
    }
    
    
    function Char(value) {
    	this.value = value;
    	this.type = 'char';
    }


    function Symbol(value) {
        this.value = value;
        this.type = 'symbol';
    }


    function List(value) {
        this.value = value;
        this.type = 'list';
    }


    function MyString(value) {
        this.value = value;
        this.type = 'string';
    }


    function MyFunction(value) {
        this.value = value;
        this.type = 'function';
    }


    function Null() {
        this.type = 'null';
    }


    function SpecialForm(value) {
        this.value = value;
        this.type = 'specialform';
    }


    function MyBoolean(value) {
        this.value = value;
        this.type = 'boolean';
    }
    
    
    function UserDefined(usertype, value) {
        this.usertype = usertype;
        this.value = value;
        this.type = 'userdefined';
    }
    
    
    function MyError(errortype, message, trace) {
    	this.errortype = errortype;
    	this.message = message;
    	this.trace = trace;
    	this.type = 'error';
    }


    return {
        'Number': function (x) {
            return new MyNumber(x)
        },
        'Char': function(x) {
        	return new Char(x);
        },
        'Function': function (x) {
            return new MyFunction(x)
        },
        'List': function (x) {
            return new List(x)
        },
        'Symbol': function (x) {
            return new Symbol(x)
        },
        'Null': function () {
            return new Null()
        },
        'SpecialForm': function (x) {
            return new SpecialForm(x);
        },
        'Boolean': function (x) {
            return new MyBoolean(x);
        },
        'UserDefined': function(x, y) {
            return new UserDefined(x, y);
        },
        'String': function(x) {
        	return new MyString(x);
        },
        'Error': function(type, message, trace) {
        	return new MyError(type, message, trace);
        }
    };

})();