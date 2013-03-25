
## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle       :=  Form(+)
    
    Form         :=  Special  |  Application  |  List  |  SYMBOL  |  NUMBER  |  STRING
    
    Special      :=  '{'  ( Define  |  Set  |  Cond  |  Lambda )  '}'

    Application  :=  '('  Form(+)  ')' 
    
    List         :=  '['  Form(*)  ']'
    
    Define       :=  'define'  SYMBOL  Form

    Set          :=  'set'  SYMBOL  Form

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
    
    STRING         :=  "[^\"]*"

    COMMENT        :=  ;[^\n]*

    SYMBOL         :=  [a-zA-Z!@#$%^&*-_=+?/<>][a-zA-Z0-9!@#$%^&*-_=+?/<>]*

    NUMBER         :=  \d*\.\d+  |  \d+\.\d*  |  \d+

