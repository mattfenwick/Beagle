define([
  "app/unparser", 
  "app/parser"
], function(unparser, parser) {

    return function () {

        module("AST unparsing");
        
        var unp = unparser.unparse,
            num = parser.ASTNumber,
            char = parser.ASTChar,
            list = parser.ASTList,
            lam = parser.Lambda,
            def = parser.Define,
            set = parser.set,
            cond = parser.Cond,
            sym = parser.Symbol,
            app = parser.Application;
    
        test("9 basic forms", function() {
            deepEqual("8234.2323", unp(num('8234.2323')), 'decimal number');
    
            deepEqual("8234", unp(num('8234')), 'integer');
            
            deepEqual("'c'", unp(char('c')), 'character');
            
            deepEqual("[]", unp(list([])), 'empty list');
            
            deepEqual("[1 '2' 3.14]", unp(list([num('1'), char('2'), num('3.14')])), 'simple list');
            
            deepEqual("(w)", unp(app([sym('w')])), "no-arg application");
            
            deepEqual("(+ 3 2)", unp(app([sym("+"), num('3'), num('2')])), 'simple application');
            
            deepEqual("{define x [1.1]}", unp(def([sym('x'), list([num('1.1')])])), 'a define');
            
            deepEqual("{set zyx 'm'}", unp(set([sym('zyx'), char('m')])), 'a set');
            
            deepEqual("{cond [[m n] [o p]] hjkl}", unp(cond([list([
                                                                list([sym('m'), sym('n')]),
                                                                list([sym('o'), sym('p')])
                                                            ]), sym('hjkl')])), "a cond");
            
            deepEqual(
                "{lambda [x y] {define z 3} (sum [x y 2 z])}", 
                unp(lam([list([sym('x'), sym('y')]), 
                         def([sym('z'), num('3')]), 
                         app([sym('sum'), list([sym('x'), sym('y'), num('2'), sym('z')])])
                        ])), 
                "a lambda"
            );
        });

    };
    
});
