
## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle       :=  Form(+)
    
    Form         :=  Special  |  Application  |  List  |  Object  |  SYMBOL  |  NUMBER  |  STRING
    
    Special      :=  ',('  SYMBOL  Form(*)  ',)'
    
    Application  :=  '('  Form(+)  ')' 
    
    List         :=  '['  Form(*)  ']'
    
    Object       :=  '{'  ( STRING  Form )(*)  '}'



## Tokens ##

    WHITESPACE     :=  \s+

    OPEN-PAREN     :=  (

    CLOSE-PAREN    :=  )

    OPEN-SQUARE    :=  [

    CLOSE-SQUARE   :=  ]

    OPEN-CURLY     :=  {

    CLOSE-CURLY    :=  }
    
    OPEN-SPECIAL   :=  ,(
    
    CLOSE-SPECIAL  :=  ,)

    STRING         :=  "[^\"]*"

    COMMENT        :=  ;[^\n]*

    SYMBOL         :=  [a-zA-Z!@#$%^&*-_=+?/<>][a-zA-Z0-9!@#$%^&*-_=+?/<>]*

    NUMBER         :=  \d*\.\d+  |  \d+\.\d*  |  \d+



## Parsing stages ##

 1. tokenization

 2. assembly of tokens to form the AST

 3. AST specialization
    - special form syntax:  check special forms' syntax subtrees
