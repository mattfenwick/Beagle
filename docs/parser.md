
## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle       :=  Form(+)
    
    Form         :=  Special  |  Application  |  List  |  Object  |  SYMBOL  |  NUMBER  |  STRING
    
    Special      :=  ',('  ( Define  |  Set!  |  Cond  |  Lambda )  ',)'

    Application  :=  '('  Form(+)  ')' 
    
    List         :=  '['  Form(*)  ']'
    
    Object       :=  '{'  ( Form  Form )(*)  '}'

    Define       :=  'define'  SYMBOL  Form

    Set!         :=  'set!'  SYMBOL  Form

    Cond         :=  'cond'  '['  ( '['  Form  Form  ']' )(*)  ']'  Form

    Lambda       :=  'lambda'  '['  SYMBOL(*)  ']'  Form(+)
    


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
