var Tokenizer = (function (Tokens, MaybeError) {
    "use strict";

/* tokenizer's missions:
     1. use 'Tokens' module's data to do String -> MaybeError [Token]
     2. keep track of input location
     3. report errors usefully
*/

    function tokenError(message, line, column, rest) {
        return MaybeError.error({
            message  :  message,
            line     :  line,
            column   :  column,
            rest     :  rest
        });
    }
    
    function countLCs(string) {
        var lines = 0,
            columns = 0,
            i;
        for(i = 0; i < string.length; i++) {
            if(string[i] === '\n') {
                lines++;
                columns = 0;
            } else {
                columns++;
            }
        }
        return [lines, columns];
    }

    //   error if a string is started but not stopped
    //   or if the input doesn't match any token definitions
    function nextToken(string, line, column) {
        var match, i, name, 
            res, count, 
            newLine, newCol;

        // 0. empty string
        if (string === "") {
            return MaybeError.zero;
        }
        
        for(i = 0; i < Tokens.PRIORITIES.length; i++) {
            name = Tokens.PRIORITIES[i];
            if(match = string.match(Tokens.REGEXES[name])) {
                // all regex matches return a pair:
                //   0: the entire match
                //   1: the 'saved' portion of the match
                count = countLCs(match[0]);
                // if we pass a newline, have to reset the column number
                newLine = line + count[0];
                newCol = ((count[0] > 0) ? 1 : column) + count[1];
                return MaybeError.pure({
                    'token' : Tokens.Token(name, match[1], {line: line, column: column}),
                    'rest'  : string.substring(match[0].length),
                    'line'  : newLine,
                    'column': newCol
                });
            }
        }
        
        if (string[0] === '"') { // this is special-cased to provide better error reporting
            return tokenError("end-of-string not found", line, column, string);
        }

        return tokenError("no tokens matched", line, column, string);
    }


    // must parse entire string to succeed
    function tokenize(string) {
        var tokens = [],
            next,
            line = 1,
            column = 1;
        while (1) {
            next = nextToken(string, line, column);
            if(next.status === 'error') {
                return MaybeError.error({
                    error :  next.value,
                    tokens:  tokens
                });
            } else if(next.status === 'failure') {
                break;
            }
            // otherwise it must be success
            tokens.push(next.value.token);
            string = next.value.rest;
            line = next.value.line;
            column = next.value.column;
        }
        return MaybeError.pure(tokens);
    }

    return {
        'tokenize'    : tokenize
    };

})(Tokens, MaybeError);