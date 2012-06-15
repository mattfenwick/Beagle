
(function(Beagle) {
	
var logic = [
    "(define not (lambda (x) (if x false true)))",
    "(define and (lambda (x y) (if x y x)))",
    "(define or (lambda (x y) (if x x y)))"
].join('');


Beagle.execAll(logic);


var lists = ['\
(define map \
  (lambda (f xs) \
    (if (= xs (list))\
        (list) \
        (cons (f (car xs)) \
              (map f (cdr xs)))))) \
\
(define filter \
  (lambda (f xs) \
    (if (= xs (list)) \
        (list) \
        (if (f (car xs)) \
            (cons (car xs) (filter f (cdr xs))) \
            (filter f (cdr xs))))))\
\
(define id \
  (lambda (x) x))'
].join('');


Beagle.execAll(lists);
	
})(Beagle);