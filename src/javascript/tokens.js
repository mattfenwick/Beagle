var Tokens = (function () {
    "use strict";

    // put capturing parentheses around that which should be saved
    //   for *all* tokens; don't worry about efficiency
    var REGEXES = {
        'whitespace'   : /^(\s+)/,
        'open-paren'   : /^(\()/,   // "("
        'close-paren'  : /^(\))/,   // "("
        'open-square'  : /^(\[)/,   // "["
        'close-square' : /^(\])/,   // "]"
        'open-curly'   : /^(\{)/,   // "{"
        'close-curly'  : /^(\})/,   // "}"
        'open-special' : /^(,\()/,  // ",("
        'close-special': /^(,\))/,  // ",)"
        'comment'      : /^;(.*)/,  /* because: 1) * is greedy; 2) . doesn't match \n, so it consumes the rest of the line */
        'float'        : /^(\d*\.\d+|\d+\.\d*)/,
        'integer'      : /^(\d+)/,
        'symbol'       : /^([a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\?\/\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\?\/\<\>]*)/,
        'string'       : /^"([^"]*)"/
    };

    // tokens' priorities
    // i.e. float must go before integer -- 
    //   otherwise integer matches only a part of what float would
    var PRIORITIES = [
        'whitespace',    'open-paren',     'open-square', 
        'close-paren',   'close-square',   'open-curly', 
        'close-curly',   'open-special',   'close-special',
        'comment',       'string',         'float',
        'integer',       'symbol'
    ];


    function token(tokentype, value, line, column) {
        if(!(tokentype in REGEXES)) {
            throw new Error("invalid token type: " + tokentype);
        }
        return {
            type       :  'token',
            tokentype  :  tokentype,
            value      :  value,
            line       :  line,
            column     :  column
        };
    }

    return {
        'Token'     :  token,
        'REGEXES'   :  REGEXES,
        'PRIORITIES':  PRIORITIES
    };

})();