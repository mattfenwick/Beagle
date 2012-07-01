## Implementation Code ##
-----

 - take a look at error-handling in `Parse.getList`, especially the `return false`
   in the middle of the while-loop:  does that make sense?  is that dead code?

 - put a logger facility in Environment:  whenever `evaluate` is called, have it
   log the function, arguments, and return value in the Environment

 - make error-checking consistent across js code

 - make error-checking complete across js code:  number and type of arguments, return values

 - improve js error message:  include all relevant information, such as the arguments that
   caused the problem

 - allow escaped `"` in string literals (as `\"`?).  need specs and tests

 - postconditions and preconditions?  (js or beagle?)

 - continue refactoring js unit tests to make them more clear, and possibly more portable

 - test primitive data types (and error-check in their constructors)

 - refactor eval/apply core.  redo closures?  apply-primitive?  apply-closure?
   don't just rely on js's apply!  give apply responsibility for function application?

 - requite whitespace before comment?

 - `tokenization` postcondtion:  never consecutive comments or whitespace tokens



## Beagle code ##
--------

 - testing library

 - user-defined data types.  how will they work with `=`?  generic equality test?
   built-in vs user-defined?  global multimethod table(s)?

 - comparison functions (`>`, `<=`, etc.)

 - exceptions?  can js exceptions be caught?

 - add macros?

 - missing:  `cond`, `let`, `quote`, `eval`, `apply`, `set!` (?)

 - special forms vs. macros.  spec and test them?  allow more special forms?

 - object system (things receieve and respond to messages)?  type reflection?

 - characters?  are strings lists of characters?

 - `define` should accept a docstring?  require it?

 - equality?  returning `nil`?

 - define `nil` in the environment?  have `nil?` function?  `nil` has error message?

 - number functions/values:  `*, /, **, e, pi, range`

 - `eval`:  where does environment come from?  have explicit environment?  make
   environment accessible from Beagle (by giving it a reference to itself)?



## Repl ##
---------

 - get `Beagle` module rock solid to allow refactoring of REPL

 - style, layout, design

 - interactive docs?

 - allow redefinition of symbols?  or use `set!`?

