
## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle      :=  Form(+)
    
    Form        :=  Special  |  Application  |  List  |  Symbol  |  Number  |  String
    
    Special     :=  '{'  ( Define  |  Set!  |  Lambda  |  Cond )  '}'
    
    Define      :=  'define'  Symbol  Form
    
    Set!        :=  'set!'  Symbol  Form
    
    Application :=  '('  Form(+)  ')' 
    
    List(T)(n)  :=  '['  T(n)  ']'
    
    Symbol      :=  [a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\?\/\!\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\?\/\!\<\>]*
    
    Number      :=  Float  |  Integer
    
    Float       :=  \d*\.\d+  |  \d+\.\d*
    
    Integer     :=  \d+
    
    String      :=  '"'  (not '"')(*)  '"'
    
    Lambda      :=  'lambda'  List(Symbol)(*)  Form(+)
    
    Pair        :=  List(Form)(2)
    
    Cond        :=  'cond'  List(Pair)(*)  Form



## Tokens ##

 - string  

   - `"[^\"]*"`
   - `"` followed by any number of non `"` chars, followed by `"`

 - comment 

   - `;[^\n]*`
   - semicolon followed by any number of non-newline chars

 - symbol

   - see above for precise definition
   - one of `!@#$%^&*-_=+?/!<>` or a letter, followed by any number of `!@#$%^&*-_=+?/!<>`
     characters or letters or numbers

 - whitespace

   - `\s+`
   - any positive number of whitespace chars

 - integer

   - `\d+`
   - any positive number of digits

 - float

   - `\d*\.\d+` or `\d+\.\d*`
   - either any number of digits, a decimal point, and at least one digit, or
     at least one digit, a decimal point, and any number of digits (so there's
     always at least one digit)

 - punctuation tokens

   - `(`:  OPEN-PAREN
   - `)`:  CLOSE-PAREN
   - `[`:  OPEN-SQUARE
   - `]`:  CLOSE-SQUARE
   - '{':  OPEN-CURLY
   - '}':  CLOSE-CURLY


## Whitespace requirements ##

 - required between any two of the atom tokens:

   - string
 
   - symbol

   - integer

   - float

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

 - newlines only matter to comments:  comment tokens are ended by newlines. 
   otherwise they count as whitespace, same as any other whitespace character



## Parsing stages ##

 1. tokenization
    - breaking text into tokens
    - checking whitespace separation

 2. assembly of tokens to form the AST (Abstract Syntax Tree)


## Interface ##

### Tokens ###

 - helper functions

   - `nextToken :: String -> Maybe (Token, String)`

 - data types
  
   - `Token`

 - core functions

   - `tokenize :: String -> Maybe [Token]`
      extract a list of tokens from a string

   - `stripTokens :: [Token] -> [Token]`
      remove comment and whitespace tokens from a list of tokens
   

### Parser ###

 - helper functions
 
   - `expandString :: String -> ASTList ASTChar`
  
   - `getAtom :: [Token] -> Maybe (ASTAtom, [Token])`

   - `getApplication :: [Token] -> Maybe (Application, [Token])`
 
   - `getList :: [Token] -> Maybe (ASTList, [Token])`

   - `getNextForm :: [Token] -> Maybe (ASTObject, [Token])`

 - data types

   - `ParseError`
   
   - `ASTChar`
   
   - `ASTList`
   
   - `ASTNumber`
   
   - `Symbol`
   
   - `Application`

 - core functions

   - `makeAst :: [Token] -> Maybe [LispObject]`

