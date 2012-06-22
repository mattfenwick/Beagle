
## Grammar ##

Loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    BeagleCode:     SExpression(+)

    SExpression:    Atom  |  List

    Atom:           String  |  Symbol

    String:         "[^\"]*"

    Symbol:         [^;\"\(\)\s]+

    List:           OPEN  SExpression(*)  CLOSE

    OPEN:           '('

    CLOSE:          ')'

Also, comments are indicated by `;` (when not in a string) and extend to the end of the line.
**I don't know how to represent comments in formal grammars.**


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
   - any positive number of chars that are not whitespace, or `(` or `)` or `"` or `;`

 - whitespace

   - `\s+`
   - any positive number of whitespace chars



## Whitespace requirements ##

 - required between these pairs of token:

   - string and symbol
 
   - string and string

   - symbol and symbol

   - symbol and string

   - string and comment

   - symbol and comment

 - optional between all other pairs of non-whitespace tokens

 - examples: `(2(` is fine because `(` is not a string or
   a symbol, so the token sequence is `OPEN symbol OPEN`

   this is fine:

        (abc "def" 1 (2(3))(4 5))

   but this is not:

        (abc "def"1)

   because there's no whitespace between the string `"def"` and the symbol `1`

 - number of whitespace characters

   - there is no difference between 1 and n whitespace characters:  each counts
     as a single whitespace token

   - there cannot be two or more consecutive whitespace tokens because
     the the first token begins at a ws char and ends at the next non-ws char



## Parsing stages ##

 1. tokenization
 2. discarding of whitespace and comment tokens
 3. assembly of tokens to form the AST (s-expressions composed of lists and atoms)
 4. AST analysis:  nodes are converted to Beagle values



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
  - `getList`:  if a list is opened but not closed, or closed but not opened
  - `getSExpression`:  if getList fails hard
  - `parse`:  if nextToken or getSExpression fails hard (this covers the case where some but not all of the input is consumed, right?)
   
 - all other failures are programmer errors -- they are believed to be impossible

  - `nextToken`:  if none of the cases match
  - `getSExpression`:  if neither an atom nor a list can be found, but the token stream is not empty



## Interface ##

 - helper functions

   - getAtom 

   - getList

   - getSExpression

   - nextToken

 - the data types
  
   - Token

   - SExpression

   - ParseError

 - core, public functions

   - tokenize:  extract a list of tokens from a string

   - stripTokens:  remove comment and whitespace tokens from a list of tokens
   
   - makeSExpressions:  assemble a list of tokens into s-expressions


