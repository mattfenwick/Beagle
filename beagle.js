'use strict';

var frontend = require('./lib/frontend'),
    unparser = require('./lib/unparser'),
    util = require('util'),
    fs = require('fs');

var commands = {
    'parse'     : frontend.parse,
    'parseAST'  : frontend.parseAST,
    'unparse'   : function (input) {
                      var astErr = frontend.parseAST(input);
                      if (astErr.status !== 'success') {
                          return astErr;
                      }
                      return unparser.unparse(astErr.value);
                  }
};

// console.log('args -- ' + process.argv);
var command = process.argv[2];

if (!commands.hasOwnProperty(command)) {
    console.log('invalid command');
    console.log('valid commands are: ' + Object.getOwnPropertyNames(commands));
    process.exit(1);
}

var f = commands[command];

var input = fs.readFileSync('/dev/stdin', {'encoding': 'utf8'});

var output = f(input);

if (typeof output === 'string') {
    process.stdout.write(output + "\n");
} else {
    process.stdout.write(util.inspect(output, {depth: null}) + "\n");
}
// process.stdout.write(JSON.stringify(output, null, 2) + "\n");

