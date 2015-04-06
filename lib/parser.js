"use strict";

var U = require('unparse-js'),
    C = U.combinators,
    Cst = U.cst;

var pos = C.position,
    literal = pos.literal, oneOf = pos.oneOf, string = pos.string,
    satisfy = pos.satisfy, alt = C.alt, seq2L = C.seq2L,
    many0 = C.many0, many1 = C.many1, optional = C.optional,
    not1 = pos.not1, seq2R = C.seq2R, error = C.error,
    node = Cst.node, cut = Cst.cut, not0 = C.not0,
    item = pos.item;

var _digit = oneOf('0123456789'), // um, does that need to be an array?

    _number = node('number',
                   ['int'    , many1(_digit)     ]),
    
    _simple = node('simple',
                   ['char', not1(oneOf('\\"'))]),
    
    _escape = node('escape',
                   ['open', literal('\\')              ],
                   ['char', cut('escape', oneOf('\\"'))]),
                   
    _string = node('string',
                   ['open' , literal('"')                ],
                   ['body' , many0(alt(_simple, _escape))],
                   ['close', cut('"', literal('"'))      ]),
    
    // not sure if the checks are right
    //   or the argument order of `check` for that matter
    _letter = satisfy(function(c) {return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');}),
    
    _special = oneOf('!@#$%^&*-_=+?/<>'),
    
    _symbol = node('symbol', 
                   ['first', alt(_letter, _special)               ],
                   ['rest' , many0(alt(_letter, _digit, _special))]),
    
    _comment = node('comment', 
                    ['open', literal(';')],
                    ['body', many0(not1(literal('\n')))]),

    _whitespace = node('whitespace',
                       ['value', many1(oneOf(' \t\n\r\f'))]),
    
    junk = many0(alt(_comment, _whitespace));

function tok(p) {
    return seq2L(p, junk);
}

var number = tok(_number),
    beagleString = tok(_string),
    symbol = tok(_symbol),
    os     = tok(literal('[')),
    cs     = tok(literal(']')),
    op     = tok(literal('(')),
    cp     = tok(literal(')')),
    oc     = tok(literal('{')),
    cc     = tok(literal('}'));
    
var form = error('undefined'), // for mutual recursion,

    list = node('list',
                ['open' , os          ],
                ['body' , many0(form) ],
                ['close', cut(']', cs)]),
    
    app = node('app',
               ['open'    , op          ],
               ['operator', form        ],
               ['args'    , many0(form) ],
               ['close'   , cut(')', cp)]),
                
    def = node('def',
               ['def'   , tok(string('def'))   ],
               ['symbol', cut('symbol', symbol)],
               ['form'  , cut('form', form)    ]),
    
    set = node('set',
               ['set'   , tok(string('set'))   ],
               ['symbol', cut('symbol', symbol)],
               ['form'  , cut('form', form)    ]),
    
    _pair = node('pair',
                 ['open'     , oc                    ],
                 ['condition', cut('condition', form)],
                 ['result'   , cut('result', form)   ],
                 ['close'    , cut('}', cc)          ]),

    cond = node('cond',
                ['cond' , tok(string('cond'))],
                ['open' , cut('{', oc)       ],
                ['pairs', many0(_pair)       ],
                ['close', cut('}', cc)       ],
                ['else' , cut('form', form)  ]),
    
    fn = node('fn',
              ['fn'        , tok(string('fn'))        ],
              ['open'      , cut('{', oc)             ],
              ['parameters', many0(symbol)           ],
              ['close'     , cut('}', cc)             ],
              ['body'      , cut('forms', many1(form))]),
    
    spec = node('special', 
                ['open' , oc                                          ], 
                ['value', cut('special form', alt(def, set, cond, fn))],
                ['close', cc                                          ]);
    
// allows mutual recursion
form.parse = alt(spec, app, list, symbol, number, beagleString).parse;

var beagle = seq2L(seq2R(junk, many0(form)), 
                   cut('unparsed input remaining', not0(item)));

module.exports = {
    'number'  :  number,
    'symbol'  :  symbol,
    'string'  :  string,
    'app'     :  app,
    'list'    :  list,
    'special' :  spec,
    'form'    :  form,
    'beagle'  :  beagle
};

