from unparse import (position, alt, seq2L, many0, many1, error, not0)
from unparse.cst import (node, cut)


literal = position.literal
oneOf = position.oneOf
string = position.string
satisfy = position.satisfy
not1 = position.not1
item = position.item

_digit = oneOf('0123456789')

_number = node('number',
              ['int'    , many1(_digit)])
    
_simple = node('simple',
               ['char', not1(oneOf('\\"'))])

_escape = node('escape',
               ['open', literal('\\')              ],
               ['char', cut('escape', oneOf('\\"'))])
                
_string = node('string',
               ['open' , literal('"')                  ],
               ['chars', many0(alt([_simple, _escape]))],
               ['close', cut('"', literal('"'))        ])

_letter = satisfy(lambda c: ('a' <= c <= 'z') or ('A' <= c <= 'Z'))

_special = oneOf('!@#$%^&*-_=+?/<>|')

_symbol = node('symbol',
               ['first', alt([_letter, _special])               ],
               ['rest' , many0(alt([_letter, _digit, _special]))])

_comment = node('comment',
                ['open', literal(';')              ],
                ['body', many0(not1(literal('\n')))])

_whitespace = node('whitespace',
                   ['value', many1(oneOf(' \t\n\r\f'))])

junk = many0(alt([_comment, _whitespace]))

def tok(p):
    return seq2L(p, junk)

number    = tok(_number)
bg_string = tok(_string)
symbol    = tok(_symbol)
os        = tok(literal('['))
cs        = tok(literal(']'))
op        = tok(literal('('))
cp        = tok(literal(')'))
oc        = tok(literal('{'))
cc        = tok(literal('}'))
    
# for mutual recursion
form = error('undefined')

bg_list = node('list',
               ['open' , os          ],
               ['body' , many0(form) ],
               ['close', cut(']', cs)])

app = node('app',
           ['open'    , op                   ],
           ['operator', cut('operator', form)],
           ['args'    , many0(form)          ],
           ['close'   , cut(')', cp)         ])
            
bg_def = node('def',
              ['def'   , tok(string('def'))   ],
              ['symbol', cut('symbol', symbol)],
              ['form'  , cut('form', form)    ])

bg_set = node('set',
              ['set'   , tok(string('set'))   ],
              ['symbol', cut('symbol', symbol)],
              ['form'  , cut('form', form)    ])

_pair = node('pair',
             ['open'     , oc                    ],
             ['condition', cut('condition', form)],
             ['result'   , cut('result', form)   ],
             ['close'    , cut('}', cc)          ])

cond = node('cond',
            ['cond' , tok(string('cond'))],
            ['open' , cut('{', oc)       ],
            ['pairs', many0(_pair)       ],
            ['close', cut('}', cc)       ],
            ['else' , cut('form', form)  ])

fn = node('fn',
          ['fn'        , tok(string('fn'))        ],
          ['open'      , cut('{', oc)             ],
          ['parameters', many0(symbol)            ],
          ['close'     , cut('}', cc)             ],
          ['forms'     , cut('forms', many1(form))])

macro = node('macro',
             ['symbol', cut('symbol', symbol    )],
             ['forms' , cut('forms', many1(form))])

special_body = alt([bg_def, bg_set, cond, fn, macro])

spec = node('special',
            ['open' , oc                               ],
            ['value', cut('special form', special_body)],
            ['close', cut('{', cc)                     ])
    
# mutual recursion
form.parse = alt([spec, app, bg_list, symbol, number, bg_string]).parse

beagle = node('beagle',
              ['open', junk        ],
              ['forms', many0(form)],
              ['close', cut('unparsed input remaining', not0(item))])
