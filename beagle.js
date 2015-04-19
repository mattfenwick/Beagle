#! node

'use strict';

var frontend = require('./lib/frontend'),
    util = require('util'),
    fs = require('fs');

var commands = {
    'parse'     : frontend.parse,
    'parseAST'  : frontend.parseAST
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

process.stdout.write(util.inspect(output, {depth: null}) + "\n");
// process.stdout.write(JSON.stringify(output, null, 2) + "\n");

