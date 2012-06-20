
function testBeagle(beagle, data) {

    module("beagle");
    
    test("single expressions", function() {
    	expect(3);

      var e1 = "(list)",
          e2 = "(cons 3 (list))",
          e3 = '(car (cdr (cons 1 (cons "blargh" (cons 3 (cons 4 (list)))))))'
          ;

      deepEqual([data.List([])], beagle.exec(e1).result);

      deepEqual([data.List([data.Number(3)])], beagle.exec(e2).result);

      deepEqual([data.String('blargh')], beagle.exec(e3).result);
    });
    
    test("multiple expressions", function() {
    	expect(2);

      var e1 = "(define x 3) (cons x (list))"
          ;

      var r = beagle.exec(e1);
      deepEqual(data.Nil(), r.result[0]);
      deepEqual(data.List([data.Number(3)]), r.result[1]);
    });

}
