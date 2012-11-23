
## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle      :=  (sepBy1  Form  ws)
    
    Form        :=  Special  |  Application  |  List  |  SYMBOL  |  NUMBER  |  STRING
    
    Special     :=  '{'  Symbol  ws  (sepBy0  Form  ws)  '}'
    
    Application :=  '('  (sepBy1  Form  ws)  ')' 
    
    List        :=  '['  (sepBy0  Form  ws)  ']'

    ws          :=  ( COMMENT  |  WHITESPACE )(+)



## Tokens ##

These are the punctuation tokens:

    OPEN-PAREN   :=  (

    CLOSE-PAREN  :=  )

    OPEN-SQUARE  :=  [

    CLOSE-SQUARE :=  ]

    OPEN-CURLY   :=  {

    CLOSE-CURLY  :=  }

And the non-punctuation tokens:

    STRING       :=  "[^\"]*"

    COMMENT      :=  ;[^\n]*

    SYMBOL       :=  [a-zA-Z\!\@\#\$\%\^\&\*\-\_\=\+\?\/\!\<\>][a-zA-Z0-9\!\@\#\$\%\^\&\*\-\_\=\+\?\/\!\<\>]*

    WHITESPACE   :=  \s+

    NUMBER       :=  \d*\.\d+  |  \d+\.\d*  |  \d+




## Whitespace ##

Beagle does not allow forms to appear without some separation, where the 
separation is any amount of consecutive whitespace and comment tokens.  Thus, Beagle is
not a totally free-format language, although it does allow a good deal of
formatting latitude.

The following examples are invalid because of adjacent forms:

    (abc(+ 3 2))

    {lambda [] 3}(+ 3 2)

    ["abc"
     "def"345]

However, these are fine:

    (abc   \t\t  \t(+           3 2))
  
    {abc ;;;; a comment
      (+ 3 2)}

    [abc
     (+
          ;; nothing but whitespace and a comment on this line
         []         3
       2]

The second group of examples are valid because all forms are separated by
some amount of whitespace and/or comments.

On the other hand, whitespace is not allowed in two places:

 1. between an opening punctuation token and the first contained form
 
 2. between a closing punctuation token and the preceding form (if there is one)

Here are some invalid examples:

    ( + x y)

    [[] ]

    {
     define
     x
     3
    }

And their valid counterparts:

    (+ x y)

    [[]]

    {define
     x
     3}


## Parsing stages ##

 1. tokenization
    - breaking text into tokens

 2. assembly of tokens to form the AST (Abstract Syntax Tree)

 3. syntax checking:
    - special form syntax
    - ???
