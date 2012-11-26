module Parser (

    Token
  , scanner

  , beagle
  , full  -- what a terrible name

) where

import TParse
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
    | Number Float
    | String String -- again, weird
    | Symbol String
  deriving (Show, Eq)
  
  
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
    pure (Number . read . concat)  <*>
    (float1 <|> float2)
  where
    float1 = commute [some digit, dot, many digit]
    float2 = commute [many digit, dot, some digit]
    dot = string "."


integer :: Parser Char Token
integer =
    pure (Number . read)  <*>
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
    firstChar = mconcat (alpha : map literal "!@#$%^&*-_=+?/<>")
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


data Form
    = Application   Form    [Form]    
    | ASpecial      Symbol  [Form]
    | AList        [Form]
    | ANumber       Float
    | AChar         Char
    | ASymbol       Symbol
  deriving (Show, Eq)


type Beagle = [Form]


astring :: Parser Token Form
astring =
    getOne >>= f
  where
    f (String s)   =  pure (AList $ map AChar s)
    f   _          =  empty


anumber :: Parser Token Form
anumber =
    getOne >>= f
  where
    f (Number f)   =  pure (ANumber f)
    f    _          =  empty


asymbol :: Parser Token Symbol
asymbol =
    getOne >>= f
  where
    f (Symbol s)   =  pure s
    f   _          =  empty


alist :: Parser Token Form
alist = 
    literal OpenSquare     >>
    commit (
    many form              >>= \es ->
    literal CloseSquare    >>
    pure (AList es)
    )


app :: Parser Token Form
app = 
    literal OpenParen        >>
    some form                >>= \(e:es) ->
    literal CloseParen       >>
    pure (Application e es)


special :: Parser Token Form
special =
    literal OpenCurly      >>
    asymbol                >>= \s ->
    many form              >>= \fs ->
    literal CloseCurly     >>
    pure (ASpecial s fs)


form :: Parser Token Form
form = 
    astring               <|> 
    anumber               <|>
    fmap ASymbol asymbol  <|>
    alist                 <|>
    app                   <|>
    special


beagle :: Parser Token [Form]
beagle = some form <* end


-- full :: String -> Either (Thing String String (String, [Token])) (Thing [Token] [Token] [Form])
full str = 
    case (getParser (scanner <* end) str) of
         Ok (rest, ts) -> case (getParser beagle . f $ ts) of
                               Ok (r1, fs) -> Right (Ok fs);
                               Fail x      -> Right (Fail x);
                               Error y     -> Right (Error y);   
         y             -> Left y;
  where
    f = filter isWs
    isWs (Whitespace _)   =  False
    isWs (Comment _)      =  False
    isWs   _              =  True
