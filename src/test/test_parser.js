function testParser(p2, tokens, testHelper) {

    module("parser");
    
    var t   =  tokens.tokenize,
        s   =  tokens.stripTokens,
        p   =  p2.parse,
        tk  =  tokens.Token;
    
    // TODO: should probably free these tests
    //   from depending so intimately on the tokenizer
    // currently they're overspecified ... 
    //   incidentally coupled
    
    test("simple", function() {
        deepEqual(p(s(t('33 21.2 "like, duh" \n abc '))), 
            {'status': 'success', 'rest': [],
             'value': [tk('integer', '33', 1, 1), tk('float', '21.2', 1, 4), 
                       tk('string', 'like, duh', 1, 9), 
                       tk('symbol', "abc", 2, 2)]});
    });
    
    test("complex", function() {
        deepEqual(p2.list(s(t('[abc 1] ['))),
            {'status': 'success', 'rest': [tk('open-square', '[')],
             'value': {type: 'listliteral', 
                       elements: [tk('symbol', 'abc'), tk('integer', '1')]}},
            'simple list literal');
        deepEqual(p2.special(s(t(' \t,(define x 22,) z'))),
            {'status': 'success', 'rest': [tk('symbol', 'z')],
             'value': {type: 'special',
                       operator: tk('symbol', 'define'), 
                       'arguments':[tk('symbol', 'x'), tk('integer', '22')]}},
            'simple special form application');
        deepEqual(p2.object(s(t('  { "abc" def "ghi  " 27  } 1'))),
            {'status':'success', 'rest': [tk('integer', '1')],
             'value': {type: 'objectliteral',
                       entries: [[tk('string', 'abc'), tk('symbol', 'def')],
                                 [tk('string', 'ghi  '), tk('integer', '27')]]}},
            'simple object literal');
        deepEqual(p2.app(s(t('(+ 1 2) 3'))),
            {'status': 'success', 'rest': [tk('integer', '3')],
             'value': {type: 'application',
                       operator: tk('symbol', '+'),
                       'arguments': [tk('integer', '1'), tk('integer', '2')]}},
            'simple function application');
        // probably not a great unit test ... vvv
        deepEqual(p(s(t('{"abc" [1] "xyz" (qrs ,(tuv xyz,))}'))),
            {'status': 'success', 'rest': [],
             'value': [{type: 'objectliteral',
                       entries: [[tk('string', 'abc'), 
                                  {type: 'listliteral', elements: [tk('integer', '1')]}],
                                 [tk('string', 'xyz'),
                                  {type: 'application',
                                   operator: tk('symbol', 'qrs'),
                                   'arguments': [{type: 'special', 
                                                  operator: tk('symbol', 'tuv'),
                                                  'arguments': [tk('symbol', 'xyz')]}]}]]}]});
    });
    
    test("error messages", function() {
        deepEqual(p(s(t('[abc 1 []'))),
            {'status': 'error', 'rest': s(t('abc 1 []')),
             'message': 'list'},
            'list literal');
        deepEqual(p(s(t('{"abc" 1'))),
            {'status': 'error', 'rest': s(t('"abc" 1')),
             'message': 'object'},
            'object literal');
        deepEqual(p(s(t('(abc 1 []'))),
            {'status': 'error', 'rest': s(t('abc 1 []')),
             'message': 'application'},
            'function application');
        deepEqual(p(s(t(',(define abc 1'))),
            {'status': 'error', 'rest': s(t('define abc 1')),
             'message': 'special-form application'},
            'special-form application');
        deepEqual(p(s(t('([abc 123'))),
            {status: 'error', rest: s(t('abc 123')),
             message: 'list'},
            'innermost error wins');
    });

}