
## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle      :=  Form(+)
    
    Form        :=  Special  |  Application  |  List  |  SYMBOL  |  NUMBER  |  STRING
    
    Special     :=  '{'  SYMBOL  Form(*)  '}'
    
    Application :=  '('  Form(+)  ')' 
    
    List        :=  '['  Form(*)  ']'



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



## Parsing stages ##

 1. tokenization

 2. assembly of tokens to form the AST

 3. AST specialization
    - special form syntax:  check special forms' syntax subtrees
