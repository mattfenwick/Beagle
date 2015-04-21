data ASTNode 
  = Application ASTNode [ASTNode]
  | AList [ASTNode]
  | ASymbol String
  | ANumber Float
  | AChar Char
  | ALambda [String] [ASTNode]
  | ACond [(ASTNode, ASTNode)] ASTNode
  | ADefine String ASTNode
  | ASetBang String ASTNode
  deriving (Show)
  
  
------------------------------------------------------
----- more intricate one with stronger typing --------

type Symbol = String


data Statement
  = Define Symbol Expr
  | SetBang Symbol Expr


-- the first Expr must evaluate to a boolean,
--   so it can only be an Application, Symbol,
--   or Cond ... oopsy-daisy!!
type Pair = (Expr, Expr)


 -- the first Expr in Application must
 --   evaluate to a function, so it can
 --   only be an Application, Lambda,
 --   Cond, or Symbol ... oopsy-daisy!!
data Expr
  = Application Expr [Expr]    
  | Lambda [Symbol]  [Form] Expr
  | Cond   [Pair]    Expr
  | List   [Expr]
  | Number Float
  | Char   Char
  | Symbol Symbol
  deriving (Show)


data Form
  = Statement Statement
  | Expression Expr


type Beagle = [Form]


{- another idea to separate those things that can be evaluated to a function from those that can't:

data Literal
  = List [Expr]
  | Number Float
  | Char Char
  deriving (Show)
  
data Combo
  = Application Combo [Expr]
  | Cond [Pair] Expr
  | List [Expr]
  | Symbol Symbol
  deriving (Show)
  
data Expression
  = Lit Literal
  | Com Combo
  deriving (Show)

... but then of course, have to check `cond` too, because it 
can't have list/lambda/number/char in first position of its pairs ...
  
-}
