# Implemented #
---------

### Special Forms ###

 - `define`
   
   1. symbol (not something that evaluates to a symbol -- an actual symbol!)
   
   2. BeagleValue
 
 - `set!`
 
   1. symbol (see 1. for `define`) that has already been `define`d
   
   2. BeagleValue
 
 - `cond` 
 
   - variadic
   
   - each argument must be a list of length 2; the first element is evaluated
     to a `Boolean`, and if true, the second element is evaluated and returned.
     This process continues through the arguments until a true condition is 
     found.  If no true condition is found, an error is (?) signaled (?).
 
 - `if` (*slated for removal; use `cond` instead)
 
 - `eval` (implemented as special form but acts like a function)
 
 - `lambda`

   - variadic
 
   1. a list of symbols (see 1. for `define`)
   
   2. one or more bodies
 
 - `quote`
 
   1. any BeagleValue
  
  
### Functions ###

 - for lists:

   - `cons :: BeagleValue -> [BeagleValue] -> [BeagleValue]`

   - `car :: [BeagleValue] -> Error BeagleValue`

   - `cdr :: [BeagleValue] -> Error BeagleValue`
 
   - `list :: (variadic) BeagleValue(s) -> [BeagleValue]`
 
   - `null? :: [BeagleValue] -> Boolean`
   
 - for numbers:

   - `+ :: Number -> Number -> Number`
 
   - `neg :: Number -> Number`
   
   - `number-< :: Number -> Number -> Boolean`
 
 - general
 
   - `prim-type :: BeagleValue -> String`
   
   - `eq? :: a -> a -> Boolean` (where `a` is one of String, Boolean, Number, Symbol)
   
   
### Constants ###

 - `true :: Boolean`
 
 - `false :: Boolean`
 
 
## Loose Ends ##

 - how to describe error conditions
 
 - are some/all/no error conditions catchable?  can a running program recover from them?


# Unimplemented #
-----------

### Special Forms ###

 - short-circuiting `and`
 
 - short-circuiting `or`

### Functions ###

 - for providing the Beagle implementation at run-time:

   - `parse :: String -> SExpression`

 - for interacting with the filesystem (probably won't get added):

   - `read :: String -> Error String` read from a file

   - `write :: String -> String -> Error ()` write to a file
   
 - basic string functions
 
   - first
   
   - rest
   
   - toString/fromString ??
   
   - comparison (`string-<`)

