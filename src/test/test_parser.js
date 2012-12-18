function testParser(parser, pc, tokens, testHelper) {

    module("parser");
    
    var p   =  parser.parse,
        t   =  tokens.Token,
        ti  =  t('integer', '33', 14, 79),
        tf  =  t('float', '21.2', 3, 22),
        ts  =  t('string', 'hi there', 8, 5),
        tc  =  t('comment', 'blargh', 27, 28),
        tsy =  t('symbol', 'abc', 19, 41),
        tsy2 = t('symbol', 'x', 19, 3),
        toc =  t('open-curly', '{', 22, 53),
        tcc =  t('close-curly', '}', 8, 17),
        tos =  t('open-square', '[', 9, 17),
        tcs =  t('close-square', ']', 15, 82),
        tosp = t('open-special', ',(', 79, 13),
        tcsp = t('close-special', ',)', 102, 11),
        top  = t('open-paren', '(', 21, 44),
        tcp  = t('close-paren', ')', 29, 18),
        pure = pc.Result.pure;
    
    test("simple", function() {
        deepEqual(p.parse([ti, tf, ts, tsy]), 
            pure({'rest': [],
                  'result': [ti, tf, ts, tsy]},
            'simple tokens are easy to parse'));
    });
    
    test("complex", function() {
        deepEqual(parser.list.parse([tos, tsy, ti, tcs, tos]), // maybe doesn't make sense to reuse tos token ... lazy!
            pure({'rest': [tos],
                  'result': {type: 'listliteral', 
                             line: 9, column: 17,
                             elements: [tsy, ti]}}),
            'simple list literal');
        deepEqual(parser.special.parse([tosp, tsy, tsy2, ti, tcsp, tsy]),
            pure({'rest': [tsy],
                  'result': {type: 'special',
                             operator: tsy, 
                             'arguments':[tsy2, ti],
                             line: 79, column: 13}}),
            'simple special form application');
        deepEqual(parser.object.parse([toc, ts, tsy, ts, tsy2, tcc, ti]),
            pure({'rest': [ti],
                  'result': {type: 'objectliteral',
                            entries: [[ts, tsy], [ts, tsy2]],
                            line: 22, column: 53}}),
            'simple object literal');
        deepEqual(parser.app.parse([top, tsy, ti, tf, tcp, ts]),
            pure({'rest': [ts],
                  'result': {type: 'application',
                             operator: tsy,
                             'arguments': [ti, tf],
                             line: 21, column: 44}}),
            'simple function application');
        // probably not a great unit test ... vvv
/*        deepEqual(p.parse([toc, ts, tos, ti, tcs, ts, top, tsy, tosp, tsy2, tf, tcsp, tcp, tcc]),
            pure({'rest': [],
                  'result': [{type: 'objectliteral',
                              line:  22, column: 53,
                              entries: [[ts, {type: 'listliteral', elements: [ti],
                                              line: 9, column: 17}],
                                        [ts,
                                         {type: 'application',
                                          line: 79, column: 13,
                                          operator: tsy,
                                          'arguments': [{type: 'special', 
                                                         line: 21, column: 44, 
                                                         operator: tsy2,
                                                         'arguments': [tf]}]}]]}]}),
            'nested example');*/
    });

    test("error messages", function() {
        var err = pc.Result.error;
        deepEqual(p.parse([toc, tsy, ti]),
            err({'rule': 'object literal', line: 22, column: 53}),
            'object literal');
        deepEqual(p.parse([tos, tsy, ti, tos, tcs]),
            err({'rule': 'list literal', line: 9, column: 17}),
            'list literal');
        deepEqual(p.parse([top, tsy, ti, tos, tcs]),
            err({'rule': 'application', line: 21, column: 44}),
            'function application');
        deepEqual(p.parse([tosp, tsy, tos, tcs]),
            err({'rule': 'special-form application', line: 79, column: 13}),
            'special-form application');
        deepEqual(p.parse([top, tos, tsy, ti]),
            err({'rule': 'list literal', line: 9, column: 17}),
            'innermost error wins');
        deepEqual(p.parse([tos, top, tsy, ti]),
            err({'rule': 'application', line: 21, column: 44}),
            'innermost wins again');
    });

}