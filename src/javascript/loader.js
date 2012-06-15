
(function(Beagle) {
    
var logic = '\
(define not \
  (lambda (x) \
    (if x false true)))\
\
(define and \
  (lambda (x y) \
    (if x y x)))\
\
(define or \
  (lambda (x y) \
    (if x x y)))\
';


Beagle.execAll(logic);


var lists = '\
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
  (lambda (x) x))\
\
(define reduce \
  (lambda (f b xs) \
    (if (= xs (list)) \
        b \
        (reduce f \
                (f b (car xs)) \
                (cdr xs)))))\
\
(define any \
  (lambda (vals) \
	(reduce or false vals)))\
\
(define all \
  (lambda (vals) \
	(reduce and true vals)))\
\
(define zip \
  (lambda (l1 l2) \
    (if (or (= (list) l1) \
            (= (list) l2)) \
        (list) \
        (cons (cons (car l1) \
                    (cons (car l2) (list))) \
              (zip (cdr l1) \
                   (cdr l2))))))\
\
(define zip-map \
  (lambda (f l1 l2) \
    (map (lambda (p) \
           (f (car p) (car (cdr p)))) \
         (zip l1 l2))))';


Beagle.execAll(lists);
    
})(Beagle);