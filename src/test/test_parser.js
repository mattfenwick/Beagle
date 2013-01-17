function testParser(parser, ast, tokens, maybeerror) {

    module("parser");
    
    var pp  =  parser.parse,
        t   =  tokens.Token,
        ti  =  t('integer', '33', 14),
        pi  =  ast.number(33, 14),
        tf  =  t('float', '21.2', 3),
        pf  =  ast.number(21.2, 3),
        ts  =  t('string', 'hi there', 8),
        ps  =  ast.list('hi there'.split('').map(ast.char), 8),
        ts2 =  t('string', 'oops', 49),
        tc  =  t('comment', 'blargh', 27),
        tsy =  t('symbol', 'abc', 19),
        psy =  ast.symbol('abc', 19),
        tsy2 = t('symbol', 'x', 19),
        psy2 = ast.symbol('x', 19),
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
    
    test("simple list, object, application", function() {
        // not sure if it's important whether I use the 'form' or 'list' parser
        deepEqual(parser.form.parse([tos, tsy, ti, tcs, tos]),
            pure({'rest': [tos],
                  'result': ast.list([psy, pi], 9)}),
            'simple list');

        deepEqual(parser.object.parse([toc, ti, tsy, tcc, ti]),
            pure({'rest': [ti],
                  'result': ast.object([[pi, psy]], 22)}),
            'simple object');

        deepEqual(parser.application.parse([top, tsy, ti, tf, tcp, ts]),
            pure({'rest': [ts],
                  'result': ast.application(psy, [pi, pf], 21)}),
            'simple function application');
    });
    
    test("special forms: define", function() {
        var def = t('symbol', 'define');
        deepEqual(parser.form.parse([tosp, def, tsy, ti, tcsp, ti]),
            pure({'rest': [ti],
                  'result': ast.define('abc', pi, 79)}),
            'define');
            
        deepEqual(parser.form.parse([tosp, def, ti, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'non-symbol in 2nd position');
        
        deepEqual(parser.form.parse([tosp, def, tsy, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'missing value');
        
        deepEqual(parser.form.parse([tosp, def, tsy, ti, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'extra arguments');
    });
    
    test("special forms: set", function() {
        var set = t('symbol', 'set');
        deepEqual(parser.form.parse([tosp, set, tsy, ti, tcsp, ti]),
            pure({'rest': [ti],
                  'result': ast.set('abc', pi, 79)}),
            'set');
        
        deepEqual(parser.form.parse([tosp, set, ti, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'non-symbol in 2nd position');
        
        deepEqual(parser.form.parse([tosp, set, tsy, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'missing value');
        
        deepEqual(parser.form.parse([tosp, set, tsy, ti, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'extra arguments');
    });
    
    test("special forms: lambda", function() {
        var lam = t('symbol', 'lambda');
        deepEqual(parser.form.parse([tosp, lam, tos, tsy, tcs, ti, tf, tcsp]),
            pure({'rest': [],
                  'result': ast.lambda(['abc'], [pi], pf, 79)}),
            'lambda');
        
        deepEqual(parser.form.parse([tosp, t('symbol', 'lambda'), tos, tsy, tsy, tcs, tf, tcsp]),
            maybeerror.error({'rule': 'special-form application', meta: 79}),
            'duplicate parameter names are forbidden');
        
        deepEqual(parser.form.parse([tosp, lam, tos, tsy, tcs, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            '0 body forms');
        
        deepEqual(parser.form.parse([tosp, lam, tos, ti, tcs, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'non-symbol parameters');
        
        deepEqual(parser.form.parse([tosp, lam, ti, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'non-list in 2nd position');
    });
    
    test("special forms: cond", function() {
        var cond = t('symbol', 'cond');
        deepEqual(parser.form.parse([tosp, cond, tos, tos, tsy, ti, tcs, tos, tsy2, tf, tcs, tcs, ti, tcsp, tf]),
            pure({'rest': [tf],
                 'result': ast.cond([[psy, pi], [psy2, pf]], pi, 79)}),
            'cond');
        
        deepEqual(parser.form.parse([tosp, cond, tos, tcs, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'missing return value');
        
        deepEqual(parser.form.parse([tosp, cond, ti, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'branches must be list');
        
        deepEqual(parser.form.parse([tosp, cond, tos, ti, tcs, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'of lists');
        
        deepEqual(parser.form.parse([tosp, cond, tos, tos, ti, tcs, tcs, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'of length 2');
         
        deepEqual(parser.form.parse([tosp, cond, tos, tcs, ti, tf, tcsp]),
            maybeerror.error({rule: 'special-form application', meta: 79}),
            'extra arguments');
    });
    
    test("special forms", function() {        
        deepEqual(
            parser.form.parse([tosp, t('symbol', 'oops'), ti, tcsp]),
            maybeerror.error({'rule': 'special-form application', meta: 79}),
            'invalid special form name');
    
        deepEqual(
            parser.form.parse([tosp, ti, tsy, tcsp]),
            maybeerror.error({'rule': 'special-form application', meta: 79}),
            'special form requires symbol as operator');
    });

    test("error messages", function() {
        var err = maybeerror.error,
            q = parser.parse.parse;
        deepEqual(q([toc, tsy, ti]),
            err({'rule': 'object', meta: 22}),
            'object');
        deepEqual(q([tos, tsy, ti, tos, tcs]),
            err({'rule': 'list', meta: 9}),
            'list');
        deepEqual(q([top, tsy, ti, tos, tcs]),
            err({'rule': 'application', meta: 21}),
            'function application');
        deepEqual(q([tosp, tsy, tos, tcs]),
            err({'rule': 'special-form application', meta: 79}),
            'special-form application');
        deepEqual(q([top, tos, tsy, ti]),
            err({'rule': 'list', meta: 9}),
            'innermost error wins');
        deepEqual(q([tos, top, tsy, ti]),
            err({'rule': 'application', meta: 21}),
            'innermost wins again');
    });

}