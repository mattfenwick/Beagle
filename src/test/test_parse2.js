function testParse2(p2, tokens, testHelper) {

    module("parse2");
	
	var t   =  tokens.tokenize,
	    s   =  tokens.stripTokens,
	    p   =  p2.parse,
		tk  =  tokens.Token;

	test("simple", function() {
	    deepEqual(p(s(t('33 21.2 abc "like, duh"'))), 
		    {'status': 'success', 'rest': [],
			 'value': [tk('integer', '33'), tk('float', '21.2'), tk('symbol', 'abc'),
			           tk('string', "like, duh")]});
	});
	
	test("complex", function() {
	    deepEqual(p2.list(s(t('[]'))),
		    {'status': 'success', 'rest': [],
			 'value': []});
	});

}