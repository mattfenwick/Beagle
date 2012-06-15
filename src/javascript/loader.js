
(function(Beagle) {
	
var logic = [
    "(define not (lambda (x) (if x false true)))",
    "(define and (lambda (x y) (if x y x)))",
    "(define or (lambda (x y) (if x x y)))"
].join('');


Beagle.execAll(logic);

	
})(Beagle);