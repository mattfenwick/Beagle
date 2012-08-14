module Parser (
    ASTNode (..)
  , Token
  , scanner
  , beagle
  , full  -- what a terrible name

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
nextToken = pany [op, cp, os, cs, ws, flt, int, str, com, sym]
  where
    op = preturn OpenParen $ literal '('
    cp = preturn CloseParen $ literal ')'
    os = preturn OpenSquare $ literal '['
    cs = preturn CloseSquare $ literal ']'
    ws = using Whitespace $ some wschar
    flt = using (Decimal . read . concat) $ alt (pall [some digit, dot, many digit]) (pall [many digit, dot, some digit])
    int = using Integer integer
    str = using (String . concat) $ pall [preturn [] $ literal '"', many $ pnot '"', preturn [] $ literal '"']
    com = using (Comment . concat) $ pall [preturn [] $ literal ';', many $ pnot '\n']
    sym = using Symbol $ some alpha
    dot = using (:[]) $ literal '.'

  
  
scanner :: Parser Char [Token]
scanner = many nextToken


-- ------------------------------------------------------------------
-- AST construction

data ASTNode 
  = Application ASTNode [ASTNode]
  | AList [ASTNode]
  | ASymbol String
  | ANumber Float
  | AChar Char
  deriving (Show)


astring :: Parser Token ASTNode
astring (String s:rest) = succeed (AList $ map AChar s) rest
astring r = pfail "unable to match 'AString'" r


anumber :: Parser Token ASTNode
anumber (Integer i:rest) = succeed (ANumber $ fromIntegral i) rest
anumber (Decimal f:rest) = succeed (ANumber f) rest
anumber r = pfail "unable to match 'ANumber'" r


asymbol :: Parser Token ASTNode
asymbol (Symbol s:rest) = succeed (ASymbol s) rest
asymbol r = pfail "unable to match 'ASymbol'" r


alist :: Parser Token ASTNode
alist = using (AList . concat) $ pall [os, forms, cs]
  where os = preturn [] $ literal OpenSquare
        forms = many form
        cs = preturn [] $ literal CloseSquare
        

app :: Parser Token ASTNode
app = using buildApp $ pseq op $ pseq (alt app asymbol) $ pseq (many form) cp
  where buildApp (_, (f, (rs, _))) = Application f rs
        op = literal OpenParen
        cp = literal CloseParen
        

form :: Parser Token ASTNode
form = pany [astring, anumber, asymbol, alist, app]


beagle :: Parser Token [ASTNode]
beagle = some form


unwrap :: (Monad m) => m (b, c) -> m c
unwrap x = x >>= (return . snd)

-- :: Monad m => (b, c) -> m c

full :: String -> Either String [ASTNode]
full str = scanner str >>= 
    (beagle . filter isNotWs . snd) >>= 
    (return . snd)
  where isNotWs (Whitespace _) = False
        isNotWs _ = True
