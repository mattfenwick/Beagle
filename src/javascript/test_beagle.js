
function testBeagle(beagle, data) {

    module("beagle");
    
    test("some expressions", function() {
    	expect(3);

      var e1 = "(list)",
          e2 = "(cons 3 (list))",
          e3 = '(car (cdr (cons 1 (cons "blargh" (cons 3 (cons 4 (list)))))))'
          ;

      deepEqual(data.List([]), beagle.lisp(e1));

      deepEqual(data.List([data.Number(3)]), beagle.lisp(e2));

      deepEqual(data.String('blargh'), beagle.lisp(e3));
    });

}
