# Implemented #
---------

### Special Forms ###

For more information about the evaluation rules for these forms, see [here](evaluation.md).

 - `define`
 
 - `set!`
 
 - `cond` 
 
 - `if` (*slated for removal; use `cond` instead)
 
 - `eval`
 
 - `lambda`
 
 - `quote`
  
  
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

