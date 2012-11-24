module RichParser (



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
    | OpenCurly
    | CloseCurly
    | OpenSquare
    | CloseSquare
    | Comma
    | Semicolon
    | Equals
    | Symbol     String
    | Keyword    String  -- <== TODO:  what's the deal with keywords, symbols, reserved words, etc.?
    | Number     Float
    | String     String
    | Comment    String
    | Whitespace String
  deriving (Show, Eq, Ord)


puncs :: [(Char, Token)]
puncs = [
    ('(', OpenParen),
    (')', CloseParen),
    ('[', OpenSquare),
    (']', CloseSquare),
    ('{', OpenCurly),
    ('}', CloseCurly),
    (',', Comma),
    (';', Semicolon),
    ('=', Equals)]


punctuation :: Parser Char Token
punctuation =
    mconcat $ map p puncs
  where
    p (c, v) = literal c *> pure v


tstring :: Parser Char Token
tstring =
    literal '"'      >>
    many (pnot '"')  >>= \cs ->
    literal '"'      >>
    pure (String cs)


tnumber :: Parser Char Token
tnumber =
    fmap (Number . read) (some $ mconcat $ map literal ['0' .. '9'])


tsymbol :: Parser Char Token
tsymbol =
    fmap Symbol (some $ satisfy $ flip elem (['a'..'z'] ++ ['A'..'Z']))


whitespace :: Parser Char Token
whitespace =
    fmap Whitespace (some $ satisfy $ flip elem " \t\n\r\f\v")


comment :: Parser Char Token
comment =
    string "//"           >>
    many (not1 newline)   >>= \cs ->
    pure (Comment cs)
  where
    newline = literal '\n' -- TODO allow other newline sequences, too


oldComment :: Parser Char Token
oldComment =
    string "/*"        >>
    many (not1 close)  >>= \cs ->
    close              >>
    pure (Comment cs)
  where
    close = string "*/"


oneToken :: Parser Char Token
oneToken =
    punctuation  <|>
    tstring      <|>
    tnumber      <|>
    tsymbol      <|>
    whitespace   <|>
    comment      <|>
    oldComment


scanner :: Parser Char [Token]
scanner = many oneToken <* end


-- ------------------------------
-- grammar

{-
func xyz(a,b,c) {
  return add(a, b, c);
// this is a comment
};

var x = [
  func y() {return 3;}, 
  13, 
  add(17, 39), 
  if(a) {b;} elif(c) {d;} else {e;}
];

x = if(y) {
      3;
    } elif(z) {
      if(a) {
        2;
      } else {
        13;
      };
    } else {
      27;
    };
-}

type Symbol = String


data Form
    = App        Form              [Form]    
    | Lambda     [Symbol]          [Form]
    | Define     Symbol            Form
    | SetBang    Symbol            Form
    | Cond       [(Form, [Form])]  [Form]
    | ANumber    Float
    | AList      [Form]
    | AChar      Char
    | ASymbol    Symbol
  deriving (Show, Eq)


type Beagle = [Form]


anumber :: Parser Token Form
anumber =
    getOne >>= f
  where
    f (Number x)  =  pure (ANumber x)
    f   _         =  empty


astring :: Parser Token Form
astring =
    getOne >>= f
  where
    f (String s)  =  pure (AList $ map AChar s)
    f   _         =  empty


asymbol :: Parser Token Symbol
asymbol = 
    getOne >>= f
  where
    f (Symbol x)  =  pure x
    f   _         =  empty


lambda :: Parser Token Form
lambda = 
    literal (Symbol "func")         >>
    literal OpenParen               >>
    sepBy0 asymbol (literal Comma)  >>= \(ss,_) ->
    literal CloseParen              >>
    block                           >>= \bs ->
    pure (Lambda ss bs)


define :: Parser Token Form
define =
    literal (Symbol "var")   >>
    asymbol                  >>= \s ->
    literal Equals           >>
    form                     >>= \f ->
    pure (Define s f)


setBang :: Parser Token Form
setBang = 
    asymbol              >>= \s ->
    literal Equals       >>
    form                 >>= \f ->
    pure (SetBang s f)


block :: Parser Token [Form]
block =
    literal OpenCurly                 >>
    some (form <* literal Semicolon)  >>= \fs ->
    literal CloseCurly                >>
    pure fs


cond :: Parser Token Form
cond = 
    literal (Symbol "if")  >>
    condition              >>= \p ->
    block                  >>= \b ->
    many elif              >>= \els ->
    elseClause             >>= \ec ->
    pure (Cond ((p,b):els) ec)
  where
    condition =
        literal OpenParen      >>
        form                   >>= \p ->
        literal CloseParen     >>
        pure p
    elif =
        literal (Symbol "elif")  >>
        condition                >>= \p ->
        block                    >>= \b ->
        pure (p, b)
    elseClause =
        literal (Symbol "else")   *>
        block

 
list :: Parser Token Form
list =
    literal OpenSquare           >>
    sepBy0 form (literal Comma)  >>= \(fs,_) ->
    literal CloseSquare          >>
    pure (AList fs)


mysymbol :: Parser Token Form
mysymbol =
    fmap ASymbol asymbol


app :: Parser Token Form
app =
    mysymbol                      >>= \s ->
    literal OpenParen             >>
    sepBy0 form (literal Comma)   >>= \(fs,_) ->
    literal CloseParen            >>
    pure (App s fs)


form :: Parser Token Form
form = 
    lambda    <|>
    cond      <|>
    define    <|>
    setBang   <|>
    list      <|>
    app       <|>
    anumber   <|>
    astring   <|>
    mysymbol
