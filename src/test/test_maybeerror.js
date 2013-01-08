
function testMaybeError(me) {

    module("maybeerror");

    function f(x) {return x + 5;}

    test("constructors", function() {
        deepEqual(new me('success', [2, 'hi']), me.pure([2, 'hi']));
        deepEqual([1,2,3], me.pure([1,2,3]).value);
        deepEqual(new me('failure'), me.zero);
        deepEqual(undefined, me.zero.value);
        deepEqual(new me('error', 'oopsy-daisy'), me.error('oopsy-daisy'));
        deepEqual('message', me.error('message').value);
    });

    test("fmap", function() {
        deepEqual(me.pure(13), me.pure(8).fmap(f));
        deepEqual(me.zero, me.zero.fmap(f));
        deepEqual(me.error('abc'), me.error('abc').fmap(f));
    });
    
    test("ap", function() {
        deepEqual(me.pure(8), me.pure(f).ap(me.pure(3)));
        deepEqual(me.zero, me.pure(f).ap(me.zero));
        deepEqual(me.zero, me.zero.ap(me.pure(3)));
    });
    
    test("bind", function() {
        function g(x) {
            if(x > 2) return me.pure(x * 2);
            return me.zero;
        }
        deepEqual(me.pure(8), me.pure(4).bind(g));
        deepEqual(me.zero, me.pure(2).bind(g));
    });
    
    test("plus", function() {
        deepEqual(me.pure(3), me.pure(3).plus(me.pure(4)), 'left-biased success');
        deepEqual(me.error('left'), me.error('left').plus(me.error('right')), 'left-biased error');
        deepEqual(me.pure(4), me.zero.plus(me.pure(4)), 'left zero: return right');
        deepEqual(me.zero, me.zero.plus(me.zero));
        deepEqual(me.pure(18), me.pure(18).plus(me.error('right')));
        deepEqual(me.error('left'), me.error('left').plus(me.pure(24)));
    });
    
    test("mapError", function() {
        deepEqual(me.pure(3), me.pure(3).mapError(f));
        deepEqual(me.zero, me.zero.mapError(f));
        deepEqual(me.error(13), me.error(8).mapError(f));
    });

}
