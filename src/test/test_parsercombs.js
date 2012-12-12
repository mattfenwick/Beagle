function testParserCombs(parserC, testHelper) {

    module("parser combinators");
    
    var item     =  parserC.item,
        sat      =  parserC.satisfy,
		literal  =  parserC.literal,
		check    =  parserC.check,
		pFail    =  parserC.pFail,
		unit     =  parserC.unit,
		fail     =  parserC.fail,
		commit   =  parserC.commit,
		either   =  parserC.either,
		bind     =  parserC.bind,
		all      =  parserC.all,
		pSuccess =  parserC.pSuccess,
		string   =  parserC.string,
		many0    =  parserC.many0,
		many1    =  parserC.many1,
		fmap     =  parserC.fmap;
    
    
    test("item", function() {
        deepEqual(item(""), pFail(""));
        deepEqual(item("abcde"), {'status': 'success', 'rest': "bcde", 'value': "a"});
        deepEqual(item([1,2,3,4]), {'status': 'success', 'rest': [2,3,4], 'value': 1});
    });
    
    test("satisfy", function() {
        deepEqual(sat(false)(""), pFail(""));
        deepEqual(sat(function(x) {return x > 3;})([4, 5, "abc"]), 
		    {'status': 'success', 'rest': [5, "abc"], 'value': 4});
		deepEqual(sat(function(y) {return y % 2 === 0;})([17, 'duh']),
		    pFail([17, 'duh']));
    });
	
	test("literal", function() {
	    deepEqual(literal('a')(""), pFail(""));
		deepEqual(literal('b')("cde"), pFail("cde"));
		deepEqual(literal('m')("matt"),
		    {status: 'success', 'rest': "att", 'value': 'm'});
		deepEqual(literal(13)([13, 79, 22]),
		    {status: 'success', 'rest': [79, 22], 'value': 13});
		deepEqual(literal([12, 13])([[12,13], 27, "abc"]),
		    {status: 'success', 'rest': [27, "abc"], 'value': [12, 13]},
			"the equality comparison should work for anything");
	});

	test("check", function() {
	    deepEqual(check(false, item)(""), pFail(""));
        deepEqual(check(false, item)(""), pFail(""));
        deepEqual(check(function(x) {return x > 3;}, item)([4, 5, "abc"]), 
		    {'status': 'success', 'rest': [5, "abc"], 'value': 4});
		deepEqual(check(function(y) {return y % 2 === 0;}, item)([17, 'duh']),
		    pFail([17, 'duh']));
	});
	
	test("unit", function() {
	    deepEqual(unit("hi there")("123abc"),
		    {status: 'success', 'rest': '123abc', 'value': 'hi there'});
	});
	
	test("fail", function() {
	    deepEqual(fail("abc123"),
		    {status: 'failed', rest: "abc123"});
	});
	
	test("commit", function() {
	    deepEqual(commit(literal('a'))("abcde"),
		    {status: 'success', rest: 'bcde', value: 'a'},
			'commit does not affect success');
		deepEqual(commit(literal('a'), "looking for a")("bcde"),
		    {status: 'error', rest: 'bcde', message: 'looking for a'},
			'commit turns failure into an error');
	});
	
	test("either", function() {
	    var parser = either(literal('a'), literal('b'));
	    deepEqual(parser("abcde"),
		    {status: 'success', 'rest': "bcde", 'value': 'a'});
	    deepEqual(parser("bcde"),
		    {status: 'success', 'rest': "cde", 'value': 'b'});
	    deepEqual(parser("cde"),
		    {status: 'failed', 'rest': "cde"});
	});
	
	test("bind", function() {
	    var p = bind(item, literal); // recognizes two of the same token
		deepEqual(p("aabcd"), {status: 'success', 'rest': 'bcd', value: 'a'});
		deepEqual(p("bbcd"), {status: 'success', 'rest': 'cd', value: 'b'});
		deepEqual(p("abcd"), pFail("bcd"));
		
		var ex = bind(item, 
		    function(x) {
			    return bind(item, 
				    function(y) {
					    return literal(x);
					});
			});
		deepEqual(ex([1,2,1,3]), {status: 'success', 'rest': [3], value: 1});
		deepEqual(ex([1,2,3,4]), pFail([3,4]));
	});
	
	test("all", function() {
	    var p = all([item, literal('x'), literal('3')]);
		deepEqual(p("ax3dyz"), pSuccess("dyz", ['a', 'x', '3']));
		deepEqual(p("bx4zzz"), pFail("4zzz"));
	});
	
	test("string", function() {
	    var p = string("public");
		deepEqual(p("publicness"), pSuccess("ness", "public"));
		deepEqual(p("pub-a-thon"), pFail("-a-thon"));
	});
	
	test("many0", function() {
	    var p = many0(literal('a'));
		deepEqual(p("bbb"), pSuccess("bbb", []));
		deepEqual(p("aaaaaabcd"), pSuccess("bcd", ['a', 'a', 'a', 'a', 'a', 'a']));
	});
	
	test("many1", function() {
	    var p = many1(literal('a'));
		deepEqual(p("bbb"), pFail("bbb"));
		deepEqual(p("aaaaaabcd"), pSuccess("bcd", ['a', 'a', 'a', 'a', 'a', 'a']));
	});
	
	test("fmap", function() {
	    var p = fmap(function(x) {return x.length;}, many1(literal('a')));
		deepEqual(p("aaabcd"), pSuccess("bcd", 3));
		deepEqual(p("bcd"), pFail("bcd"));
	});

}