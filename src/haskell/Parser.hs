module Parser (

    Token
  , scanner

--  , beagle
--  , full  -- what a terrible name

) where

import MParse
import Instances
import Classes
import Prelude hiding (foldr, foldl, (>>=), (>>), fmap, fail)
  
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
  | Decimal Float
  | String String -- again, weird
  | Symbol String
  deriving (Show, Eq)
  
  
myReader :: String -> Integer
myReader [] = 0
myReader xs = read xs


separators :: [(Char, Token)]
separators = 
    [('(', OpenParen),
     (')', CloseParen),
     ('[', OpenSquare),
     (']', CloseSquare),
     ('{', OpenCurly),
     ('}', CloseCurly)]


punctuation :: Parser Char Token
punctuation = mconcat $ map f separators
  where
    f (c, v) = literal c *> pure v


whitespace :: Parser Char Token
whitespace =
    pure Whitespace   <*>
    some wschar
  where
    wschar = satisfy (flip elem " \t\n\r\f")


digit :: Parser Char Char
digit = mconcat $ map literal ['0' .. '9']


float :: Parser Char Token
float =
    pure (Decimal . read . concat)  <*>
    (float1 <|> float2)
  where
    float1 = commute [some digit, dot, many digit]
    float2 = commute [many digit, dot, some digit]
    dot = string "."


integer :: Parser Char Token
integer =
    pure (Decimal . read)  <*>
    some digit


str :: Parser Char Token
str =
    literal '"'       >>
    many (pnot '"')   >>= \cs ->
    literal '"'       >>
    pure (String cs)


comment :: Parser Char Token
comment =
    some (literal ';')  >>
    many (pnot '\n')    >>= \cs ->
    pure (Comment cs)


symbol :: Parser Char Token
symbol =
    firstChar             >>= \c ->
    many restChar         >>= \cs ->
    pure (Symbol (c:cs))
  where
    firstChar = alpha <|> mconcat (map literal "!@#$%^&*-_=+?/<>")
    restChar = digit <|> firstChar
    alpha = mconcat $ map literal (['a' .. 'z'] ++ ['A' .. 'Z'])


nextToken :: Parser Char Token
nextToken = 
    punctuation      <|>
    whitespace       <|>
    float            <|>
    integer          <|>
    str              <|>
    comment          <|>
    symbol
  
  
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
astring =
    getOne >>= f
  where
    f (String s)   =  pure (AList $ map AChar s)
    f   _          =  empty


anumber :: Parser Token Expr
anumber =
    getOne >>= f
  where
    f (Decimal f)   =  pure (ANumber f)
    f    _          =  empty


asymbol :: Parser Token Symbol
asymbol =
    getOne >>= f
  where
    f (Symbol s)   =  pure s
    f   _          =  empty


-- probably nearly every use of 'ws' should actually be '(some ws)' to allow multiple whitespace/comment tokens
ws :: Parser Token Token
ws = satisfy f
  where f (Whitespace _) = True
        f (Comment _) = True
        f _ = False


alist :: Parser Token Expr
alist = 
    literal OpenSquare     >> 
    sepBy0 expr (some ws)  >>= \(es, _) ->
    literal CloseSquare    >>
    pure (AList es)


myList :: Parser Token a -> Parser Token [a]
myList p = 
    literal OpenSquare    >>
    sepBy0 p (some ws)    >>= \(ps,_) ->
    literal CloseSquare   >>
    pure ps


app :: Parser Token Expr
app = 
    literal OpenParen        >>
    sepBy1 expr (some ws)    >>= \(e:es,_) ->
    literal CloseParen       >>
    pure (Application e es)


expr :: Parser Token Expr
expr = mconcat [astring, anumber, fmap ASymbol asymbol, alist, app, cond, lambda]


cond :: Parser Token Expr
cond =
    literal OpenCurly        >>
    literal (Symbol "cond")  >>
    some ws                  >>
    myList pair              >>= \ps ->
    some ws                  >>
    expr                     >>= \el ->
    literal CloseCurly       >>
    pure (Cond ps el)
  where
    pair =
        literal OpenSquare   >>
        expr                 >>= \e1 ->
        some ws              >>
        expr                 >>= \e2 ->
        literal CloseSquare  >>
        pure (e1, e2)


-- {lambda [x y] (what) (+ x y)}
-- TODO:  oops, last form has to be an expression
lambda :: Parser Token Expr
lambda = 
    literal OpenCurly          >>
    literal (Symbol "lambda")  >>
    some ws                    >>
    myList asymbol             >>= \syms ->
    some ws                    >>
    sepBy1 form (some ws)      >>= \(bs,_) ->
    literal CloseCurly         >>
    f syms (init bs) (last bs)
  where
    f xs ys (Expression e)  =  pure (Lambda xs ys e)   -- TODO:  oops, partial functions
    f _  _  _               =  empty


define :: Parser Token Statement
define =
    literal OpenCurly          >>
    literal (Symbol "define")  >>
    some ws                    >>
    asymbol                    >>= \s ->
    some ws                    >>
    expr                       >>= \e ->
    literal CloseCurly         >>
    pure (Define s e)


setbang :: Parser Token Statement
setbang = 
    literal OpenCurly          >>
    literal (Symbol "set!")    >>
    some ws                    >>
    asymbol                    >>= \s ->
    some ws                    >>
    expr                       >>= \e ->
    literal CloseCurly         >>
    pure (SetBang s e)

        

form :: Parser Token Form
form = 
    fmap Expression expr      <|>
    fmap Statement statement


statement :: Parser Token Statement
statement = 
    define   <|>
    setbang

 
beagle :: Parser Token [Form]
beagle = fmap fst $ sepBy1 form (some ws)


unwrap :: (Monad' m) => m (b, c) -> m c
unwrap = fmap snd



full :: String -> Maybe [Form]
full str = 
    getParser scanner str     >>= 
    (getParser beagle . snd)  >>= 
    (return . snd)
