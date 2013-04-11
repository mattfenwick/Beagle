define(function() {

    function Input(elem, cont) {
        this.elem = elem;
        this.controller = cont;

        var self = this;
        this.elem.bind('keydown', function(e) {
            var code = (e.keyCode ? e.keyCode : e.which);
            if( code == 13 ) { // why isn't this triple-equal?
                var str = self.elem.val();
                self.controller.evalString(str);
            }
            return true;
        });
    }
    
    Input.prototype.reset = function(result) {
        if ( result.status === 'success' ) {
            this.elem.val("");
        }
    };
    
    return Input;
    
});
    