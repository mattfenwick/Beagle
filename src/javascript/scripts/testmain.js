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
    var mods = Array.prototype.slice.call(arguments);
    mods.map(function(mod, ix) {
        try {
            mod();
        } catch (e) {
            test("uncaught exception", function() {
                deepEqual(0, e.message);
            });
        }
    });
});
