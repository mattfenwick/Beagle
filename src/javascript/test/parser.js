define(["app/parser", "app/ast", "app/tokens", "libs/maybeerror"], function(parser, ast, tokens, maybeerror) {

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
        top  = t('open-paren', '(', 21),
        tcp  = t('close-paren', ')', 29),
        pure = maybeerror.pure;
        
    function good(value, rest, state) {
        return pure({'result': value, 'rest': rest, 'state': state});
    }
    
    test("simple tokens -> simple parse nodes", function() {
        propEqual(pp.parse([ti, tf, ts, tsy]), 
            good([pi, pf, ps, psy], []));
    });
    
    test("simple list and application", function() {
        // not sure if it's important whether I use the 'form' or 'list' parser
        propEqual(parser.form.parse([tos, tsy, ti, tcs, tos]),
            good(ast.list([psy, pi], 9), [tos]),
            'simple list');

        propEqual(parser.application.parse([top, tsy, ti, tf, tcp, ts]),
            good(ast.application(psy, [pi, pf], 21), [ts]),
            'simple function application');
    });
    
    test("special forms: define", function() {
        var def = t('symbol', 'define');
        propEqual(parser.form.parse([toc, def, tsy, ti, tcc, ti]),
            good(ast.define('abc', pi, 22), [ti]),
            'define');
            
        propEqual(parser.form.parse([toc, def, ti, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'non-symbol in 2nd position');
        
        propEqual(parser.form.parse([toc, def, tsy, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'missing value');
        
        propEqual(parser.form.parse([toc, def, tsy, ti, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'extra arguments');
    });
    
    test("special forms: set", function() {
        var set = t('symbol', 'set');
        propEqual(parser.form.parse([toc, set, tsy, ti, tcc, ti]),
            good(ast.set('abc', pi, 22), [ti]),
            'set');
        
        propEqual(parser.form.parse([toc, set, ti, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'non-symbol in 2nd position');
        
        propEqual(parser.form.parse([toc, set, tsy, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'missing value');
        
        propEqual(parser.form.parse([toc, set, tsy, ti, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'extra arguments');
    });
    
    test("special forms: lambda", function() {
        var lam = t('symbol', 'lambda');
        propEqual(parser.form.parse([toc, lam, tos, tsy, tcs, ti, tf, tcc]),
            good(ast.lambda(['abc'], [pi], pf, 22), []),
            'lambda');
        
        propEqual(parser.form.parse([toc, t('symbol', 'lambda'), tos, tsy, tsy, tcs, tf, tcc]),
            maybeerror.error({'rule': 'special-form application', meta: 22}),
            'duplicate parameter names are forbidden');
        
        propEqual(parser.form.parse([toc, lam, tos, tsy, tcs, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            '0 body forms');
        
        propEqual(parser.form.parse([toc, lam, tos, ti, tcs, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'non-symbol parameters');
        
        propEqual(parser.form.parse([toc, lam, ti, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'non-list in 2nd position');
    });
    
    test("special forms: cond", function() {
        var cond = t('symbol', 'cond');
        propEqual(parser.form.parse([toc, cond, tos, tos, tsy, ti, tcs, tos, tsy2, tf, tcs, tcs, ti, tcc, tf]),
            good(ast.cond([[psy, pi], [psy2, pf]], pi, 22), [tf]),
            'cond');
        
        propEqual(parser.form.parse([toc, cond, tos, tcs, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'missing return value');
        
        propEqual(parser.form.parse([toc, cond, ti, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'branches must be list');
        
        propEqual(parser.form.parse([toc, cond, tos, ti, tcs, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'of lists');
        
        propEqual(parser.form.parse([toc, cond, tos, tos, ti, tcs, tcs, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'of length 2');
         
        propEqual(parser.form.parse([toc, cond, tos, tcs, ti, tf, tcc]),
            maybeerror.error({rule: 'special-form application', meta: 22}),
            'extra arguments');
    });
    
    test("special forms", function() {        
        propEqual(
            parser.form.parse([toc, t('symbol', 'oops'), ti, tcc]),
            maybeerror.error({'rule': 'special-form application', meta: 22}),
            'invalid special form name');
    
        propEqual(
            parser.form.parse([toc, ti, tsy, tcc]),
            maybeerror.error({'rule': 'special-form application', meta: 22}),
            'special form requires symbol as operator');
    });

    test("error messages", function() {
        var err = maybeerror.error,
            q = parser.parse.parse;
        propEqual(q([tos, tsy, ti, tos, tcs]),
            err({'rule': 'list', meta: 9}),
            'list');
        propEqual(q([top, tsy, ti, tos, tcs]),
            err({'rule': 'application', meta: 21}),
            'function application');
        propEqual(q([toc, tsy, tos, tcs]),
            err({'rule': 'special-form application', meta: 22}),
            'special-form application');
        propEqual(q([top, tos, tsy, ti]),
            err({'rule': 'list', meta: 9}),
            'innermost error wins');
        propEqual(q([tos, top, tsy, ti]),
            err({'rule': 'application', meta: 21}),
            'innermost wins again');
    });

});
