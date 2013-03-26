
var NEW_TESTS = [
    "test/ast",
    "test/tokens",
    "test/environment",
    "test/data",
/*    "test/functions",
    "test/tokenizer",
    "test/parser",
    "test/evaluate",
    "test/beagle",*/
//    "test/macros",
//    "test/unparser" 
];

require(NEW_TESTS, function() {
    module("testmain");
    
    test("testmain", function() {
        ok(1, "modules loaded");
    });
});
