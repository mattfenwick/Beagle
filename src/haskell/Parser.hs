module Parser (
    Token
  , scanner
--  , beagle
--  , full  -- what a terrible name

) where

import Control.Monad.Error -- for Monad instance of Either
import MyCombinators
  
-- -----------------------------------------------------------------
-- Tokens


data Token 
  = OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare
  | OpenCurly
  | CloseCurly
  | Whitespace String
  | Comment String
  | Integer Integer -- wow, that looks weird ... is the first one a constructor and the second one a type ???
  | Decimal Float
  | String String -- again, weird
  | Symbol String
  deriving (Show, Eq)
  
  
myReader :: String -> Integer
myReader [] = 0
myReader xs = read xs


nextToken :: Parser Char Token
nextToken = pany [op, cp, os, cs, oc, cc, ws, flt, int, str, com, sym]
  where
    op = preturn OpenParen   $ literal '('
    cp = preturn CloseParen  $ literal ')'
    os = preturn OpenSquare  $ literal '['
    cs = preturn CloseSquare $ literal ']'
    oc = preturn OpenCurly   $ literal '{'
    cc = preturn CloseCurly  $ literal '}'
    ws = using Whitespace $ some wschar
    flt = using (Decimal . read . concat) $ alt (pall [some digit, dot, many digit]) (pall [many digit, dot, some digit])
    int = using Integer integer
    str = using (String . concat) $ pall [preturn [] $ literal '"', many $ pnot '"', preturn [] $ literal '"']
    com = using (Comment . concat) $ pall [preturn [] $ literal ';', many $ pnot '\n']
    sym = using (Symbol . (uncurry (:))) (firstChar <*> restChars)
    dot = using (:[]) $ literal '.'
    firstChar = alt alpha $ pany $ map literal "!@#$%^&*-_=+?/<>"
    restChars = many (alt firstChar digit)

  
  
scanner :: Parser Char [Token]
scanner = many nextToken


-- ------------------------------------------------------------------
-- AST construction
type Symbol = String


data Statement
  = Define Symbol Expr
  | SetBang Symbol Expr
  deriving (Show, Eq)


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
  | AList   [Expr]
  | ANumber Float
  | AChar   Char
  | ASymbol Symbol
  deriving (Show, Eq)


data Form
  = Statement Statement
  | Expression Expr
  deriving (Show, Eq)


type Beagle = [Form]


astring :: Parser Token Expr
astring (String s:rest) = succeed (AList $ map AChar s) rest
astring r = pfail "unable to match 'AString'" r


anumber :: Parser Token Expr
anumber (Integer i:rest) = succeed (ANumber $ fromIntegral i) rest
anumber (Decimal f:rest) = succeed (ANumber f) rest
anumber r = pfail "unable to match 'ANumber'" r


asymbol :: Parser Token Expr
asymbol (Symbol s:rest) = succeed (ASymbol s) rest
asymbol r = pfail "unable to match 'ASymbol'" r


-- probably nearly every use of 'ws' should actually be '(some ws)' to allow multiple whitespace/comment tokens
ws :: Parser Token Token
ws = satisfy f
  where f (Whitespace _) = True
        f (Comment _) = True
        f _ = False


infixl 0 *>
a *> b = ignoreLeft a b


infixl 0 <*
a <* b = ignoreRight a b


infixl 0 <*>
a <*> b = pseq a b


alist :: Parser Token Expr
alist = literal OpenSquare *> using AList (exprs <* literal CloseSquare)
  where exprs = using fst $ separatedBy0 expr (some ws)      


myList :: Parser Token a -> Parser Token [a]
myList p = literal OpenSquare *> ps <* literal CloseSquare
  where ps = using fst $ separatedBy0 p (some ws)


app :: Parser Token Expr
app = using (\(x:xs) -> Application x xs) (op *> exprs <* cp)
  where exprs = using fst $ separatedByOne expr (some ws)
        op = literal OpenParen
        cp = literal CloseParen


expr :: Parser Token Expr
expr = pany [astring, anumber, asymbol, alist, app, cond, lambda]


cond :: Parser Token Expr
cond = using (uncurry Cond) (oc *> conSym *> ws *> (myList pair) <* ws <*> expr <* cc)
  where pair = os *> expr <*> (ws *> expr <* cs)
        oc = literal OpenCurly
        cc = literal CloseCurly
        os = literal OpenSquare
        cs = literal CloseSquare
        conSym = literal $ Symbol "cond"


lambda :: Parser Token Expr -- {lambda [x y] (what) (+ x y)}
lambda = using f (oc *> lamSym *> ws *> (myList sym) <* ws <*> bodies <* cc)
  where oc = literal OpenCurly
        cc = literal CloseCurly
        os = literal OpenSquare
        cs = literal CloseSquare
        f (syms, (forms, ex)) = Lambda syms forms ex
        sym = using (\(Symbol x) -> x) $ satisfy (\x -> case x of (Symbol x) -> True; _ -> False) -- wow, that's awful:  matching on 'Symbol' like that in the 'using' function
        lamSym = literal $ Symbol "lambda"
        bodies inp = (using fst $ separatedByOne form (some ws)) inp >>= 
          \(rest, result) -> case (last result) of (Expression e) -> Right (rest, (init result, e));
                                                   _ -> Left "lambda must end with an expression"


define :: Parser Token Statement
define = using (uncurry Define) (op *> def *> ws *> sym <* ws <*> expr <* cp)
  where op = literal OpenCurly
        cp = literal CloseCurly
        sym = using (\(Symbol x) -> x) $ satisfy (\x -> case x of (Symbol x) -> True; _ -> False)
        def = literal $ Symbol "define"


setbang :: Parser Token Statement
setbang = using (uncurry SetBang) (op *> set *> ws *> sym <* ws <*> expr <* cp)
  where op = literal OpenCurly
        cp = literal CloseCurly
        sym = using (\(Symbol x) -> x) $ satisfy (\x -> case x of (Symbol x) -> True; _ -> False)
        set = literal $ Symbol "set!"
        

form :: Parser Token Form
form = alt (using Expression expr) (using Statement statement)


statement :: Parser Token Statement
statement = alt define setbang

 
beagle :: Parser Token [Form]
beagle = some form

unwrap :: (Monad m) => m (b, c) -> m c
unwrap = liftM snd



full :: String -> Either String [Form]
full str = scanner str >>= 
    (beagle . snd) >>= 
    (return . snd)
