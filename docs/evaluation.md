# Evaluation #
--------------

## Abstract syntax ## 

    Beagle     :  Expression(+)
    
    Expression :  Application  |  List  |  Symbol  |  Number  |  Char
    
    Application:  '('  Expression Expression(*)  ')'
    
    List       :  '['  Expression(*)  ']'
    
    Symbol     :  /^[a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\?\/\!\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\?\/\!\<\>]*/
    
    Number     :  /^(?:\d*\.\d+|\d+\.\d*)/   |   /^\d+/
    
    Char       :  (no literal form -- but can be extracted from strings)



## Informal semantics ##

 - Beagle[[Expression]]: evaluates each expression, in order, and returns ... what?  the last one?
   to start off, an environment is passed in ... each expression may change the environment, which
   is then passed to the next one ... should probably add in concept of statements here
   
 - Expression[[E E(*)]]: this is called an *application*.  The first expression must evaluate to either
   a function or a special form; otherwise it's a type error.  For the first element:
   
     - case 1: special form -- the arguments are passed unevaluated and it is responsible for evaluating 
       them and coming up with a result.
   
     - case 2: function, the arguments are evaluated in order (possibly changing the environment, 
       which is passed sequentially), and then the function is applied to the arguments.  The return
       value is then the return value of the application of the function/special form.
   
 - Expression[[List]]: each of the elements of the list literal are evaluated in order (possibly
   changing the environment, which is threaded through them), and the output elements returned
   in a list.
   
 - Expression[[Symbol]]: the symbol is looked up in its enclosing lexical environments and its bound
   value returned.  If no binding is found, that's an error (compile/runtime?)
   
 - Expression[[Number]]: a number object is constructed with the value of the number node
 
 - Expression[[Char]]: a character object is constructed with the value of the char node


## Special forms ##

 - `lambda`

   - variadic
 
   - a list of symbols
   
   - one or more body expressions
 
 - `cond`
 
   - variadic
   
   - each argument must be a list of length 2; the first element is evaluated
     to a `Boolean`, and if true, the second element is evaluated and returned.
     This process continues through the arguments until a true condition is 
     found.  If no true condition is found, an error is (?) signaled (?).
 
 - `define`
   
   1. symbol
   
   2. BeagleValue
 
 - `set!`
 
   1. symbol that has already been `define`d in an enclosing lexical scope
   
   2. BeagleValue
