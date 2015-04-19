# Beagle #
======

Beagle is a minimalist dialect of Lisp inspired by McCarthy's original Lisp, 
lambda calculus, and 
[M-Lisp](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.40.4948).


# Getting Started #

There are two quick and easy ways to start using Beagle:

 1. try out the [online REPL](http://mattfenwick.github.com/Beagle)

 2. pull the github repo, and start a local REPL by opening `src/javascript/index.html`
    in a web browser
    

## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle       :=  Form{+}
    
    Form         :=  Special  |  Application  |  Object  |  List  |  SYMBOL  |  NUMBER  |  STRING
    
    Special      :=  '{'  ( Define  |  Set  |  Cond  |  Function )  '}'

    Application  :=  '('  Form{+}  ')' 
    
    Object       :=  '{'  key/val(*)  '}'
        key/val  :=  STRING  ':'  Form
    
    List         :=  '['  Form{*}  ']'
    
    Define       :=  'def'  SYMBOL  Form

    Set          :=  'set'  SYMBOL  Form

    Cond         :=  'cond'  '{'  pair{*}  '}'  Form
        pair     :=  '{'  Form  Form  '}'

    Function     :=  'fn'  '{'  SYMBOL{*}  '}'  Form{+}
    


## Tokens ##

Whitespace and comments can occur in any amount between any tokens:

    WHITESPACE     :=  \s+

    OPEN-PAREN     :=  (

    CLOSE-PAREN    :=  )

    OPEN-SQUARE    :=  [

    CLOSE-SQUARE   :=  ]

    OPEN-CURLY     :=  {

    CLOSE-CURLY    :=  }
    
    STRING         :=  '"'  ( simple  |  escape )(*)  '"'
        simple     :=  [^\\"]
        escape     :=  '\\'  [\\"]
        
    COMMENT        :=  /;[^\n]*/

    SYMBOL         :=  /[a-zA-Z!@#$%^&*-_=+?/<>][a-zA-Z0-9!@#$%^&*-_=+?/<>]*/

    NUMBER         :=  /\d+/


# Evaluation #
--------------

## Abstract syntax ##

    Beagle        :=  Form(+)
    
    Form          :=  Statement  |  Expr 
    
    Statement     :=  Define  |  Set
    
    Define        :=  Symbol  Expr
    
    Set           :=  Symbol  Expr
    
    Expr          :=  Application  |  Cond  |  Lambda  |  List(Expr)(*)  |  Symbol  |  Number  |  Char
    
    Application   :=  Expr(+) 
    
    Cond          :=  List(Pair)(*)  Expr
    
    Pair          :=  List(Expr)(2)
    
    Lambda        :=  List(Symbol)(*)  Form(*)  Expr
    
    List(Type)(n) :=  Type(n)



## Informal semantics ##

 - `Beagle[[Expr(+)]]`: evaluates each expression, in order, and returns ... what?  the last one? ...
   along with the environment, which may have been augmented or modified during evaluation
   to start off, an environment is passed in ... each expression may change the environment, which
   is then passed to the next one ... should probably add in concept of statements here
   
 - `Application[[Expr(+)]]`: The first expression must evaluate to a function; otherwise it's a type error.
   The arguments are evaluated in order (possibly changing the environment, 
   which is passed sequentially), and then the function is applied to the arguments.  The return
   value is then the return value of the function.
   
 - `Cond[[Pair(*) Expr]]`: for each of the pairs, the first element is evaluated to a boolean.  If it
   doesn't evaluate to a boolean, it's a type error.  If the boolean is true, the second element is evaluated
   and returned.  If it's false, evaluation continues with the next pair.  If no true first element is found,
   the the last expression is evaluated and its result returned.  Evaluation may change the environment, thus the pairs 
   are evaluated in order and the environment is threaded through them.
   
 - `Lambda[[List(Symbol)(*) Form(*) Expr]]`: this constructs and returns a function which is a closure over
   its lexical environment.  When evaluated, the closure creates local bindings for its symbols, evaluates
   its body forms in order, possibly modifying the local environment and any lexically enclosing environments,
   and evaluates and returns the last expression.
   
 - `List[[Expr(*)]]`:  this constructs a list by evaluating each element in order and placing the results
   in a new list.
   
 - `Expr[[Symbol]]`: the symbol is looked up in its enclosing lexical environments and its bound
   value returned.  If no binding is found, that's an error (compile/runtime?)
   
 - `Expr[[Number]]`: a number object is constructed with the value of the number node
 
 - `Expr[[Char]]`: a character object is constructed with the value of the char node
 
 - `Statement[[Define | Set]](env)`:  statements are solely for modifying the lexically enclosing
   environments.  They have no return values, and are not expressions.
   
 - `Define[[Symbol Expr]]`:  if the symbol is not bound in the current environment, the expression is 
   evaluated and a new binding created for the symbol.  If it is bound, this is an error.  Bindings
   can shadow bindings of identical names in lexically enclosing environments; this enables safe and
   easy to understand alpha-substitution of lambda expressions.
   
 - `Set[[Symbol Expr]]`:  if the symbol is bound in **any** lexically enclosing environment (including
   the current one), the expression is evaluated and the 'closest' enclosing binding is modified to
   that new value.  If it's not bound, that is an error.

