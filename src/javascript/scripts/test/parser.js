define([
  "app/parser",
  "app/ast",
  "unparse-js/maybeerror"
], function(P, A, M) {

    module("parser");
    
    return function () {
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
            pure = maybeerror.pure,
            error = maybeerror.error,
            empty = {};
            
        function good(value, rest, state) {
            return pure({'result': value, 'rest': rest, 'state': state});
        }
        
        test("simple tokens -> simple parse nodes", function() {
            propEqual(pp.parse([ti, tf, ts, tsy]), 
                good([pi, pf, ps, psy], []));
        });
        
        test("list", function() {
            propEqual(parser.form.parse([tos, tsy, ti, tcs, top], empty),
                good(ast.list([psy, pi], 9), [top], empty),
                'simple list');
    
            propEqual(parser.form.parse([tos, tsy], empty),
                error({'message': 'unable to match list', 'open': tos}));
                
            propEqual(parser.form.parse([tos, tsy, tcc], empty),
                error({'message': 'unable to match list', 'open': tos, 'expected close': tcc}));
        });
        
        test("application", function() {
            propEqual(parser.form.parse([top, tsy, ti, tf, tcp, ts], empty),
                good(ast.application(psy, [pi, pf], 21), [ts], empty),
                'simple function application');
            
            propEqual(parser.form.parse([top, tsy, ti], empty),
                error({'message': 'unable to match application', 'open': top}));
                
            propEqual(parser.form.parse([top, tsy, ti, tcc], empty),
                error({'message': 'unable to match application', 'open': top, 'expected close': tcc}));
        });
        
        test("special forms: define", function() {
            var def = t('symbol', 'define');
            propEqual(parser.form.parse([toc, def, tsy, ti, tcc, ti], empty),
                good(ast.define('abc', pi, 22), [ti], empty),
                'define');
                
            propEqual(parser.form.parse([toc, def, ti, tf, tcc], empty),
                error({rule: 'define', open: toc, message: 'expected symbol'}),
                'non-symbol in 2nd position');
            
            propEqual(parser.form.parse([toc, def, tsy, tcc], empty),
                error({rule: 'define', message: 'expected form', open: toc}),
                'missing value');
            
            propEqual(parser.form.parse([toc, def, tsy, ti, tf, tcc], empty),
                error({message: 'unable to match special form', 
                       'expected close': tf, open: toc}),
                'extra arguments');
        });
        
        test("special forms: set", function() {
            var set = t('symbol', 'set');
            propEqual(parser.form.parse([toc, set, tsy, ti, tcc, ti]),
                good(ast.set('abc', pi, 22), [ti]),
                'set');
            
            propEqual(parser.form.parse([toc, set, ti, tf, tcc]),
                error({rule: 'set', open: toc, message: 'expected symbol'}),
                'non-symbol in 2nd position');
            
            propEqual(parser.form.parse([toc, set, tsy, tcc]),
                error({rule: 'set', message: 'expected form', open: toc}),
                'missing value');
            
            propEqual(parser.form.parse([toc, set, tsy, ti, tf, tcc]),
                error({message: 'unable to match special form', 
                       'expected close': tf, open: toc}),
                'extra arguments');
        });
        
        test("special forms: lambda", function() {
            var lam = t('symbol', 'lambda');
            propEqual(parser.form.parse([toc, lam, toc, tsy, tcc, ti, tf, tcc]),
                good(ast.lambda(['abc'], [pi], pf, 22), []),
                'lambda');
            
            propEqual(parser.form.parse([toc, t('symbol', 'lambda'), toc, tsy, tsy, tcc, tf, tcc]),
                error({'rule': 'lambda', symbols: ["abc", "abc" ], 'message': 'repeated symbol in parameter list'}),
                'duplicate parameter names are forbidden');
            
            propEqual(parser.form.parse([toc, lam, toc, tsy, tcc, tcc]),
                error({message: 'expected body forms', rule: 'lambda', open: toc}),
                '0 body forms');
            
            propEqual(parser.form.parse([toc, lam, toc, ti, tcc, tf, tcc]),
                error({message: 'non-symbol in parameter list', token: ti,
                       rule: 'lambda'}),
                'non-symbol parameters');
            
            propEqual(parser.form.parse([toc, lam, ti, tf, tcc]),
                error({rule: 'lambda', 'message': 'expected parameter list', open: toc}),
                'non-parameter-list in 2nd position');
        });
        
        test("special forms: cond", function() {
            var cond = t('symbol', 'cond');
            propEqual(parser.form.parse([toc, cond, toc, toc, tsy, ti, tcc, toc, tsy2, tf, tcc, tcc, ti, tcc, tf]),
                good(ast.cond([[psy, pi], [psy2, pf]], pi, 22), [tf]),
                'cond');
            
            propEqual(parser.form.parse([toc, cond, toc, tcc, tcc]),
                error({rule: 'cond', message: 'expected else form', open: toc}),
                'missing else value');
            
            propEqual(parser.form.parse([toc, cond, ti, tf, tcc]),
                error({rule: 'cond', message: 'expected predicate/result pairs', open: toc}),
                'branches must be list');
            
            propEqual(parser.form.parse([toc, cond, tos, ti, tcs, tf, tcc]),
                error({rule: 'cond', message: 'expected predicate/result pairs', open: toc}),
                'of lists');
            
            propEqual(parser.form.parse([toc, cond, tos, tos, ti, tcs, tcs, tf, tcc]),
                error({rule: 'cond', message: 'expected predicate/result pairs', open: toc}),
                'of length 2');
             
            propEqual(parser.form.parse([toc, cond, toc, tcc, ti, tf, tcc]),
                error({message: 'unable to match special form', 
                       open: toc, 'expected close': tf}),
                'extra arguments');
        });
        
        test("special forms", function() {        
            propEqual(
                parser.form.parse([toc, t('symbol', 'oops'), ti, tcc]),
                error({message: 'unable to match special form', open: toc}),
                'invalid special form name');
        
            propEqual(
                parser.form.parse([toc, ti, tsy, tcc]),
                error({message: 'unable to match special form', open: toc}),
                'special form requires symbol as operator');
        });
    
        test("error messages", function() {
            var q = parser.parse.parse;
    
            propEqual(q([tos, tsy]),
                error({'message': 'unable to match list', open: tos}),
                'open brace without matching close');
            propEqual(q([top, tsy, ti, tf, tcp, ts, tcs]),
                error({'message': 'unmatched close brace', token: tcs}),
                'close brace without matching open');
            propEqual(q([tos, tsy, tcc]),
                error({'message': 'unable to match list', open: tos, 'expected close': tcc}),
                'corresponding open and close braces of different types');
        });

    };
    
});
