# Evaluation #
--------------

Beagle has a small number of special forms that each has its own evaluation 
rule.  Other than these, evaluation proceeds "normally".

The typical Beagle evaluation scheme is a mutually recursive cycle of 
evaluation and function application.

We'll use a function called `beval` (for *B*eagle *eval*) that performs 
evaluation, mapping Beagle objects to Beagle objects.


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
           
           
           
## Normal evaluation ##

Some Beagle objects are self-evaluating.  This means that `beval x = x`.
Here are the self-evaluating objects:

 - strings
 
 - numbers
 
 - boolean

 - functions
 
 - special forms
 
 
However, symbols are not self-evaluating.  To evaluate a symbol, the name
is looked up in the current lexical environment.  If a binding for the symbol
is found in the innermost scope, the value is returned.  Otherwise, the 
procedure repeats itself with the next enclosing scope.  If no binding for
the symbol is found anywhere, an error is **returned??/thrown??/signaled??**.


 
Other: how are instances of user-defined types evaluated???
 
 
 
So that covers single objects.  Beagle also evaluates lists.  To evaluate a list,
the first element of the list is evaluated.  (Note that the empty list can not
be evaluated -- doing so is an error).  There are two valid possibilities for
the first element:

 - it's a special form:  the appropriate evaluation rules are found and used
 
 - it's a function:  all arguments to the function are evaluated and the function
   is applied to its arguments
   
Thus, the first element *must* evaluate to a function or a special form.

