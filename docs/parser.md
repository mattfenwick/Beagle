
## Grammar ##

Concrete syntax, loosely using [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

    Beagle       :=  Form{+}
    
    Form         :=  Special  |  Application  |  Object  |  List  |  SYMBOL  |  NUMBER  |  STRING
    
    Special      :=  '{'  ( Define  |  Set  |  Cond  |  Function )  '}'

    Application  :=  '('  Form{+}  ')' 
    
    Object       :=  '{'  key/val(*)  '}'
        key/val  :=  STRING  ':'  Form
    
    List         :=  '['  Form{*}  ']'
    
    Define       :=  'def'  SYMBOL  Form

    Set          :=  'set'  SYMBOL  Form

    Cond         :=  'cond'  '{'  pair{*}  '}'  Form
        pair     :=  '{'  Form  Form  '}'

    Function     :=  'fn'  '{'  SYMBOL{*}  '}'  Form{+}
    


## Tokens ##

Whitespace and comments can occur in any amount between any tokens:

    WHITESPACE     :=  \s+

    OPEN-PAREN     :=  (

    CLOSE-PAREN    :=  )

    OPEN-SQUARE    :=  [

    CLOSE-SQUARE   :=  ]

    OPEN-CURLY     :=  {

    CLOSE-CURLY    :=  }
    
    STRING         :=  '"'  ( simple  |  escape )(*)  '"'
        simple     :=  [^\\"]
        escape     :=  '\\'  [\\"]
        
    COMMENT        :=  /;[^\n]*/

    SYMBOL         :=  /[a-zA-Z!@#$%^&*-_=+?/<>][a-zA-Z0-9!@#$%^&*-_=+?/<>]*/

    NUMBER         :=  /\d+/

