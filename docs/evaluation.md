# Evaluation #
--------------

## Abstract syntax ## 

    Beagle     :  Expression(+)
    
    Expression :  Application  |  ListLiteral  |  Symbol
    
    Application:  Expression Expression(*) 
    
    ListLiteral:  Expression(*)
    
    Symbol     :  -- this is an atom --



## Informal semantics ##

 - Beagle[[Expression]]: evaluates each expression, in order, and returns ... what?  the last one?
   to start off, an environment is passed in ... each expression may change the environment, which
   is then passed to the next one ... should probably add in concept of statements here
   
 - Expression[[E E(*)]]: this is called application.  The first expression must evaluate to either
   a function or a special form; otherwise it's a type error.  If it's a special form, the arguments
   are passed unevaluated and it is responsible for evaluating them and coming up with a result.
   If it's a function, the arguments are evaluated in order (possibly changing the environment, 
   which is passed sequentially), and then the function is applied to the arguments.  The return
   value is then the return value of the application of the function/special form.
   
 - Expression[[ListLiteral]]: each of the elements of the list literal are evaluate in order (possibly
   changing the environment, which is threaded through them), and the output elements returned
   in a list.
   
 - Expression[[Symbol]]: the symbol is looked up in its enclosing lexical environments and its bound
   value returned.  If no binding is found, that's an error (compile/runtime?)


## Special forms ##

 - `lambda`

   - variadic
 
   - a list of symbols (see 1. for `define`)
   
   - one or more bodies
 
 - `cond`
 
   - variadic
   
   - each argument must be a list of length 2; the first element is evaluated
     to a `Boolean`, and if true, the second element is evaluated and returned.
     This process continues through the arguments until a true condition is 
     found.  If no true condition is found, an error is (?) signaled (?).
 
 - `define`
   
   1. symbol (not something that evaluates to a symbol -- an actual symbol!)
   
   2. BeagleValue
 
 - `set!`
 
   1. symbol (see 1. for `define`) that has already been `define`d
   
   2. BeagleValue
 
 - `quote`
 
   1. any BeagleValue
 
 - `eval`  **Note** although this is implemented as a special form, 
           it behaves like a function.
           
           
           
## Evaluation of values##

Is this necessary?  This may not be possible in future versions.
