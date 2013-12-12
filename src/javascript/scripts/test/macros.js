define(["app/macros", "app/parser"], function(macros, parser) {

    module("macros");
    
    return function () {
        var list = parser.ASTList,
            lam = parser.Lambda,
            def = parser.Define,
            sym = parser.Symbol,
            app = parser.Application;
        
        test("let", function() {
            
            // {let [[a b]]
            //   (+)}
            //
            // -- to --
            //
            // ({lambda []
            //    {define a b}
            //    (+)})
            var i = macros.Let([Parser.ASTList([Parser.Symbol('a'), Parser.Symbol('b')])], 
                               Parser.Application([Parser.Symbol('+')])).expand(),
                o = app([
                    lam([list([]),
                        def([sym('a'), sym('b')]),
                        app([sym('+')])
                    ])
                ]);
            
            deepEqual(i, o, "let-expansion");
        });

    };
    
});
