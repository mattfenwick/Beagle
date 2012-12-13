var Parse2 = (function(P) {
    
    function tokentype(type) {
        return P.satisfy(
            function(t) {
                return t.type === type;
            });
    }

    var pNumber = P.either(tokentype('integer'), tokentype('float'));
    
    var pString = tokentype('string');

    var pSymbol = tokentype('symbol');
    
    function openCommit(open, middle, close, message) {
        return P.seq2R(open, 
                       P.commit(P.seq2L(middle, 
                                        close),
                                message));
    }
    
    var pObject = openCommit(
        tokentype('open-curly'),
        P.many0(P.all([pString, pForm])), // PROBLEM:  mutual recursion doesn't work !!!
        tokentype('close-curly'),         // with vars -- have to use functions !!
        'object');
    
    var pList = openCommit(
        tokentype('open-square'),
        P.many0(pForm),
        tokentype('close-square'),
        'list');
    
    var pApp = openCommit(
        tokentype('open-paren'),
        P.all([pForm, P.many0(pForm)]), // could just do P.many1(pForm), but this keeps 1st separate from rest
        tokentype('close-paren'),
        'application');
    
    var pSpec = openCommit(
        tokentype('open-special'),
        P.all([pSymbol, P.many0(pForm)]),
        tokentype('close-special'),
        'special-form application');
    
    var pForm = P.any([pSpec, pApp, pList, pObject, pSymbol, pNumber, pString]);
    
    var parser = P.many0(pForm);
    
    return {
        'number'  :  pNumber,
        'symbol'  :  pSymbol,
        'string'  :  pString,
        'app'     :  pApp,
        'list'    :  pList,
        'object'  :  pObject,
        'special' :  pSpec,
        'form'    :  pForm,
        'op'      :  openCommit, // TODO:  remove
        'parse'   :  parser
    };

})(ParserCombs);