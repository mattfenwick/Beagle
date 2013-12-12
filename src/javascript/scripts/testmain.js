require.config({
    paths: {
        'unparse-js': '../bower_components/unparse-js/'
    }
});

var NEW_TESTS = [
    "test/ast"        ,
    "test/environment",
    "test/data"       ,
    "test/functions"  ,
    "test/parser"     ,
    "test/buildast"   ,
    "test/evaluate"   ,
    "test/beagle"     ,
    "test/model"
//    "test/macros",
//    "test/unparser" 
];

require(NEW_TESTS, function() {
    module("testmain");
    
    test("testmain", function() {
        ok(1, "modules loaded");
    });
});
