function testParser(parser, ptree, tokens, maybeerror) {

    module("parser");
    
    var pp  =  parser.parse,
        t   =  tokens.Token,
        p   =  ptree,
        ti  =  t('integer', '33', 14),
        pi  =  p.number(33, 14),
        tf  =  t('float', '21.2', 3),
        pf  =  p.number(21.2, 3),
        ts  =  t('string', 'hi there', 8),
        ps  =  p.string('hi there', 8),
        ts2 =  t('string', 'oops', 49),
        tc  =  t('comment', 'blargh', 27),
        tsy =  t('symbol', 'abc', 19),
        psy =  p.symbol('abc', 19),
        tsy2 = t('symbol', 'x', 19),
        psy2 = p.symbol('x', 19),
        toc =  t('open-curly', '{', 22),
        tcc =  t('close-curly', '}', 8),
        tos =  t('open-square', '[', 9),
        tcs =  t('close-square', ']', 15),
        tosp = t('open-special', ',(', 79),
        tcsp = t('close-special', ',)', 102),
        top  = t('open-paren', '(', 21),
        tcp  = t('close-paren', ')', 29),
        pure = maybeerror.pure;
    
    test("simple tokens -> simple parse nodes", function() {
        deepEqual(pp.parse([ti, tf, ts, tsy]), 
            pure({'rest': [],
                  'result': [pi, pf, ps, psy]}));
    });
    
    test("complex", function() {
        // not sure if it's important whether I use the 'form' or 'list' parser
        deepEqual(parser.form.parse([tos, tsy, ti, tcs, tos]),
            pure({'rest': [tos],
                  'result': p.list([psy, pi], 9)}),
            'simple list literal');
        deepEqual(parser.form.parse([tosp, tsy, tsy2, ti, tcsp, tsy]),
            pure({'rest': [tsy],
                  'result': p.special('abc', [psy2, pi], 79)}),
            'simple special form application');
        deepEqual(parser.object.parse([toc, ts, tsy, tcc, ti]),
            pure({'rest': [ti],
                  'result': p.object({'hi there': psy}, 22)}),
            'simple object literal');
        deepEqual(parser.app.parse([top, tsy, ti, tf, tcp, ts]),
            pure({'rest': [ts],
                  'result': p.app(psy, [pi, pf], 21)}),
            'simple function application');
        // probably not a great unit test ... vvv
        deepEqual(parser.form.parse([toc, ts, tos, ti, tcs, ts2, top, tsy, tosp, tsy2, tf, tcsp, tcp, tcc]),
            pure({'rest': [],
                  'result': p.object({'hi there': p.list([pi], 9),
                                      'oops': p.app(psy, [p.special('x', [pf], 79)], 21)}, 22)}),
            'nested example');
    });

    test("error messages", function() {
        var err = maybeerror.error,
            q = parser.parse.parse;
        deepEqual(q([toc, tsy, ti]),
            err({'rule': 'object literal', meta: 22}),
            'object literal');
        deepEqual(q([tos, tsy, ti, tos, tcs]),
            err({'rule': 'list literal', meta: 9}),
            'list literal');
        deepEqual(q([top, tsy, ti, tos, tcs]),
            err({'rule': 'application', meta: 21}),
            'function application');
        deepEqual(q([tosp, tsy, tos, tcs]),
            err({'rule': 'special-form application', meta: 79}),
            'special-form application');
        deepEqual(q([top, tos, tsy, ti]),
            err({'rule': 'list literal', meta: 9}),
            'innermost error wins');
        deepEqual(q([tos, top, tsy, ti]),
            err({'rule': 'application', meta: 21}),
            'innermost wins again');
    });

}