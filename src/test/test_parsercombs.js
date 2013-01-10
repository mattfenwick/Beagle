function testParserCombs(parser, maybeerror) {

    module("parser combinators");
    
    var item     =  parser.item,
        sat      =  parser.satisfy,
        literal  =  parser.literal,
        zero     =  maybeerror.zero,
        error    =  maybeerror.error,
        pure     =  maybeerror.pure,
        all      =  parser.all,
        string   =  parser.string,
        any      =  parser.any;
    
    function myPure(value, rest) {
        return parser.pure(value).parse(rest);
    }
    
    
    test("item", function() {
        deepEqual(item.parse(""), zero);
        deepEqual(item.parse("abcde"), myPure('a', 'bcde'));
        deepEqual(item.parse([1,2,3,4]), myPure(1, [2,3,4]));
    });

    test("check", function() {
        deepEqual(item.check(false).parse(""), zero);
        deepEqual(item.check(function(x) {return x > 3;}).parse([4, 5, "abc"]), 
            myPure(4, [5, "abc"]));
        deepEqual(item.check(function(y) {return y % 2 === 0;}).parse([17, 'duh']),
            zero);
    });
    
    test("satisfy", function() {
        deepEqual(sat(false).parse(""), zero);
        deepEqual(sat(function(x) {return x > 3;}).parse([4, 5, "abc"]), 
            myPure(4, [5, "abc"]));
        deepEqual(sat(function(y) {return y % 2 === 0;}).parse([17, 'duh']),
            zero);
    });
    
    test("literal", function() {
        deepEqual(literal('a').parse(""), zero);
        deepEqual(literal('b').parse("cde"), zero);
        deepEqual(literal('m').parse("matt"),
            myPure('m', "att"));
        deepEqual(literal(13).parse([13, 79, 22]),
            myPure(13, [79, 22]));
        function g(l, r) {
            if(l.length !== r.length) {
                return false;
            }
            for(var i = 0; i < l.length; i++) {
                if(l[i] !== r[i]) {
                    return false;
                }
            }
            return true;
        }
        deepEqual(literal([12, 13], g).parse([[12,13], 27, "abc"]),
            myPure([12, 13], [27, "abc"]),
            "the equality comparison must work for anything");
        function f(x, y) {
            return x.b === y.b;
        }
        deepEqual(literal({b: 2, c: 3}, f).parse([{b: 2, c: 311}, 17]),
            myPure({b: 2, c: 311}, [17]));
    });
    
    test("pure", function() {
        deepEqual(parser.pure("hi there").parse("123abc"),
            myPure('hi there', '123abc'));
    });
    
    test("zero", function() {
        deepEqual(parser.zero.parse("abc123"), zero);
    });
    
    test("plus", function() {
        var p = literal('a').plus(literal('b'));
        deepEqual(p.parse("abcde"), myPure('a', 'bcde'));
        deepEqual(p.parse("bcde"), myPure('b', 'cde'));
        deepEqual(p.parse("cde"), zero);
        deepEqual(literal('a').plus(parser.get.bind(parser.error)).parse("xyz"),
            maybeerror.error("xyz"));
    });
    
    test("commit", function() {
        var err = parser.error;
        deepEqual(literal('a').commit('blegg').parse("bcde"),
            err('blegg').parse("bcde"),
            'commit turns failure into an error');
        deepEqual(literal('a').commit('???').parse("abcde"),
            myPure('a', 'bcde'),
            'but does not affect success');
        deepEqual(parser.error(123).commit('ouch!').parse('abcde'),
            maybeerror.error(123),
            'or error');
    });
    
    test("bind", function() {
        var p = item.bind(literal); // recognizes two of the same token
        deepEqual(p.parse("aabcd"), myPure('a', 'bcd'));
        deepEqual(p.parse("bbcd"), myPure('b', 'cd'));
        deepEqual(p.parse("abcd"), zero);
        
        var ex = item.bind(function(x) {
            return item.bind(function(y) {
                return literal(x);
            });
        });
        deepEqual(ex.parse([1,2,1,3]), myPure(1, [3]));
        deepEqual(ex.parse([1,2,3,4]), zero);
    });
    
    test("all", function() {
        var p = all([item, literal('x'), literal('3')]);
        deepEqual(all([]).parse('abc'), myPure([], 'abc'), "all's identity");
        deepEqual(all([literal('2')]).parse("2345"), myPure(['2'], '345'));
        deepEqual(p.parse("ax3dyz"), myPure(['a', 'x', '3'], "dyz"));
        deepEqual(p.parse("bx4zzz"), zero);
    });
    
    test("fmap", function() {
        var p = literal(3).fmap(function(x) {return x + 15;});
        deepEqual(p.parse([3,4,5]), myPure(18, [4,5]));
        deepEqual(p.parse("bcd"), zero);
    });
    
    test("seq2L", function() {
        var p = literal('a').seq2L(literal("b"));
        deepEqual(p.parse("abcdefg"), myPure('a', 'cdefg'));
        deepEqual(p.parse("acefg"), zero);
    });
    
    test("seq2R", function() {
        var p = literal('a').seq2R(literal("b"));
        deepEqual(p.parse("abcdefg"), myPure('b', 'cdefg'));
        deepEqual(p.parse("acefg"), zero);
    });
    
    test("string", function() {
        var p = string("public");
        deepEqual(p.parse("publicness"), myPure('public', 'ness'));
        deepEqual(p.parse("pub-a-thon"), zero);
    });
    
    test("many0", function() {
        var p = literal('a').many0();
        deepEqual(p.parse("bbb"), myPure([], 'bbb'));
        deepEqual(p.parse("aaaaaabcd"), myPure(['a', 'a', 'a', 'a', 'a', 'a'], 'bcd'));
        deepEqual(parser.error('abc').many0().parse("abc"),
            maybeerror.error("abc"),
            'must respect errors');
    });
    
    test("many1", function() {
        var p = literal('a').many1();
        deepEqual(p.parse("bbb"), zero);
        deepEqual(p.parse("aaaaaabcd"), myPure(['a', 'a', 'a', 'a', 'a', 'a'], 'bcd'));
        deepEqual(parser.error('abc').many1().parse("abc"),
            maybeerror.error("abc"),
            'must respect errors');
    });
    
    test("any", function() {
        var p = parser.any([literal('a'), literal('b'), string("zyx")]);
        deepEqual(p.parse("aq123"), myPure('a', 'q123'));
        deepEqual(p.parse("zyx34534"), myPure('zyx', '34534'));
        deepEqual(p.parse("zy123"), zero);
        deepEqual(parser.any([literal('a'), parser.error(13)]).parse('cde'),
            maybeerror.error(13));
    });
    
    test("app", function() {
        var app = parser.app;
        deepEqual(app(function(x,y,z) {return x + z;}, item, literal(-1), item).parse([18, -1, 27, 3, 4]), 
            myPure(45, [3, 4]));
        deepEqual(app(undefined, item, literal(2)).parse([1,3,4,5]), zero);
        deepEqual(app(undefined, item, literal(1).commit('blah')).parse([1,2,3,4]),
            maybeerror.error('blah'), 
            'app must respect errors');
    });
    
    test("optional", function() {
        deepEqual(literal('a').optional().parse('bcde'),
            myPure(undefined, 'bcde'));
        deepEqual(literal('a').optional().parse('abcd'),
            myPure('a', 'bcd'));
    });
    
    test("error", function() {
        var err = parser.error;
        deepEqual(literal('a').seq2R(err('qrs')).parse('abcd'),
            maybeerror.error('qrs'));
        deepEqual(literal('a').seq2R(err('tuv')).parse('bcd'), zero);
    });
    
    test("mapError", function() {
        var err = parser.error;
        deepEqual(err([89, 22]).parse([2,3,4]).mapError(function(e) {
            return {e: e, length: e.length};
        }), maybeerror.error({e: [89, 22], length: 2}));
    });

    // not0, not1
}