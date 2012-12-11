function testParserCombs(parserC, testHelper) {

    module("parser combinators");
    
    var item = parserC.item,
        sat = parserC.satisfy;
    
    
    test("item", function() {
        deepEqual(item(""), false);
        deepEqual(item("abcde"), ["bcde", "a"]);
        deepEqual(item([1,2,3,4]), [[2,3,4], 1]);
    });
    
    test("satisfy", function() {
        deepEqual(sat(false, item)(""), false);
        deepEqual(sat(
    });

}