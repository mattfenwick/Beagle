function testParserCombs(parserC, testHelper) {

    module("parser combinators");
    
    var item     =  parserC.Parser.item,
        sat      =  parserC.Parser.satisfy,
        literal  =  parserC.Parser.literal,
        zero     =  parserC.Result.zero,
        error    =  parserC.Result.error,
        pure     =  parserC.Result.pure,
        all      =  parserC.Parser.all,
        string   =  parserC.Parser.string,
        any      =  parserC.Parser.any;
    
    function myPure(value, rest) {
        return parserC.Parser.pure(value).parse(rest);
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
        deepEqual(parserC.Parser.pure("hi there").parse("123abc"),
            myPure('hi there', '123abc'));
    });
    
    test("parser zero", function() {
        deepEqual(parserC.Parser.zero.parse("abc123"), zero);
    });
    
    test("result plus", function() {
        var rs = parserC.Result.pure,
            rf = zero,
            re = parserC.Result.error;
        deepEqual(rs(3).plus(rs(4)), rs(3), "left-biased in success");
        deepEqual(rs(3).plus(re("error")), rs(3), "even if right is an error");
        deepEqual(zero.plus(rs(18)), rs(18));
        deepEqual(zero.plus(re("uh-oh")), re("uh-oh"));
        deepEqual(re("left").plus(re("right")), re("left"), "left-biased in error");
        deepEqual(zero.plus(zero), zero);
    });
    
    test("parser plus", function() {
        var parser = literal('a').plus(literal('b'));
        deepEqual(parser.parse("abcde"),
            myPure('a', 'bcde'));
        deepEqual(parser.parse("bcde"),
            myPure('b', 'cde'));
        deepEqual(parser.parse("cde"),
            zero);
        deepEqual(literal('a').plus(parserC.Parser.get.bind(parserC.Parser.error)).parse("xyz"),
            parserC.Result.error("xyz"));
    });
    
    test("commit", function() {
        var err = parserC.Parser.error;
        deepEqual(literal('a').commit('blegg').parse("bcde"),
            err('blegg').parse("bcde"),
            'commit turns failure into an error');
        deepEqual(literal('a').commit('???').parse("abcde"),
            myPure('a', 'bcde'),
            'but does not affect success');
        deepEqual(parserC.Parser.error(123).commit('ouch!').parse('abcde'),
            parserC.Result.error(123),
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
        deepEqual(parserC.Parser.error('abc').many0().parse("abc"),
            parserC.Result.error("abc"),
            'must respect errors');
    });
    
    test("many1", function() {
        var p = literal('a').many1();
        deepEqual(p.parse("bbb"), zero);
        deepEqual(p.parse("aaaaaabcd"), myPure(['a', 'a', 'a', 'a', 'a', 'a'], 'bcd'));
        deepEqual(parserC.Parser.error('abc').many1().parse("abc"),
            parserC.Result.error("abc"),
            'must respect errors');
    });
    
    test("any", function() {
        var p = parserC.Parser.any([literal('a'), literal('b'), string("zyx")]);
        deepEqual(p.parse("aq123"), myPure('a', 'q123'));
        deepEqual(p.parse("zyx34534"), myPure('zyx', '34534'));
        deepEqual(p.parse("zy123"), zero);
        deepEqual(parserC.Parser.any([literal('a'), parserC.Parser.error(13)]).parse('cde'),
            parserC.Result.error(13));
    });
    
    test("app", function() {
        var app = parserC.Parser.app;
        deepEqual(app(function(x,y,z) {return x + z;}, item, literal(-1), item).parse([18, -1, 27, 3, 4]), 
            myPure(45, [3, 4]));
        deepEqual(app(undefined, item, literal(2)).parse([1,3,4,5]), zero);
        deepEqual(app(undefined, item, literal(1).commit('blah')).parse([1,2,3,4]),
            parserC.Result.error('blah'), 
            'app must respect errors');
    });
    
    test("optional", function() {
        deepEqual(literal('a').optional().parse('bcde'),
            myPure(undefined, 'bcde'));
        deepEqual(literal('a').optional().parse('abcd'),
            myPure('a', 'bcd'));
    });
    
    test("error", function() {
        var err = parserC.Parser.error;
        deepEqual(literal('a').seq2R(err('qrs')).parse('abcd'),
            parserC.Result.error('qrs'));
        deepEqual(literal('a').seq2R(err('tuv')).parse('bcd'), zero);
    });
    
    test("mapError", function() {
        var err = parserC.Parser.error;
        deepEqual(err([89, 22]).parse([2,3,4]).mapError(function(e) {
            return {e: e, length: e.length};
        }), parserC.Result.error({e: [89, 22], length: 2}));
    });

    // not0, not1
}