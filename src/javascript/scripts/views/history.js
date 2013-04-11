define(function() {

    function History(elem) {
        this.elem = elem;
    }
    
    History.prototype.setScroll = function() {
        var h = this.elem;
        h.scrollTop(h.prop("scrollHeight"));
    };
    
    History.prototype.add = function(string, result) {
        var h = this.elem; // needs to look at whether it's an error or success
        h.append("<li><pre>" + "&gt; " + string + "</pre></li>");
        h.append("<li>" + JSON.stringify(result) + "</li>");
        this.setScroll();
    };
    
    return History;

});