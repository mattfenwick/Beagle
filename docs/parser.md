
## Tokens ##

 - open 

   - `(`
   - single open parentheses

 - close

   - `)`
   - single close parentheses

 - string  

   - `"[^\"]*"`
   - `"` followed by any number of non `"` chars, followed by `"`

 - comment 

   - `;[^\n]*`
   - semicolon followed by any number of non-newline chars

 - symbol

   - `[^;\"\(\)\s]+`
   - any number of chars that are not whitespace, or `(` or `)` or `"` or `;`



## Whitespace ##

 - required: between all consecutive strings and symbols
    (note that this probably isn't yet implemented)

   this is fine:

        (abc "def" 1 (2(3))(4 5))

   but this is not:

        (abc "def"1)

   because there's no whitespace between the string `"def"` and the symbol `1`

 - optional:  between an open or

   note that it's okay to have `(2(` because `(` is not a string or
   a symbol:  the token sequence is `OPEN symbol OPEN`

 - number of whitespace characters

   - there is no difference between 1 and n whitespace characters



## Parsing stages ##

 1. tokenization
 2. assembly of tokens to form the AST (s-expressions composed of lists and atoms)
 3. AST analysis:  nodes are converted to Beagle values



## Data types ##

 - Token

   - type:  see Section A for a complete list of recognized types
   - value: the string from the original input stream

 - SExpression

   - type:  'list', 'string', or 'symbol'
   - value: a list of SExpressions (if a list), or a string (if a string or symbol)

 - ParseError

   - message:  describes what happened and why
   - value:  optional context of the error



## Failure modes ##

 - total functions (none)

 - functions that fail softly -- returning false
  
  - `nextToken`:  if the input is empty
  - `getAtom`:  if the input is empty or the first element is not an atom
  - `getList`:  if the input is empty or the first element is an atom
  - `getSExpression`:  if the input is empty

 - functions that fail hard -- throwing ParseErrors

  - `nextToken`:  if a string is opened but not closed
  - `tokenize`:  if nextToken fails hard
  - `getList`:  if a list is opened but not closed
  - `getSExpression`:  if getList fails hard
  - `parse`:  if nextToken or getSExpression fails hard (this covers the case where some but not all of the input is consumed, right?)
   
 - all other failures are programmer errors -- they are believed to be impossible

  - `nextToken`:  if none of the cases match
  - `getSExpression`:  if neither an atom nor a list can be found, but the token stream is not empty



## Public interface ##

    Token       :: (data type)
    SExpression :: (data type)
    ParseError  :: (data type)

    parse       :: String -> [SExpression]
    tokenize    :: String -> [Token]

    ?? should have a public function for [Token] -> [SExpression]

