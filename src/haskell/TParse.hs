module TParse (

    Parser(..)
  , Thing(..)

  , getOne
  , check
  , satisfy
  , literal
  
  , (<*)
  , (*>)
  , some
  , many
  
  , optional
  , sepBy1
  , sepBy0
  
  , end
  , not1
  , pnot
  , pnone
  , string
  
  , commit

  , (<?>)

) where

import Classes
import Instances () -- what does this do?
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import Thing


-- ideas for dealing with errors:
--   1. in a 'commit', record where the commit started
--      and how far the parsing got before it failed
--   2. maybe forget the idea of having all 'stack trace'
--      be automatically managed, but instead, whenever
--      the user adds a context message to a parser, push
--      that message on to a stack if the parser fails
--   3. or just return all possible failure points (even
--      ones that were successfully backtracked from ??)
--      and leave it up to the programmer to use 'commit'
--      to pare down the tree of possibilities
--   4. but remember that if I want to be able to read the
--      position information, and there's more than 1
--      parsing stage, I'll have to preserve that information
--      to be able to deliver a decent error message
--   5. ways to do that:  a) augment all tokens with column,
--      line info -- but how does that interact with multi-
--      stage parsers?? -- b) keep track of the columns, 
--      lines inside the parser -- same ? as for a ...

mapFail :: (b -> d) -> Thing a b c -> Thing a d c
mapFail f (Fail x)    =  Fail (f x)
mapFail _ (Error a)   =  Error a
mapFail _ (Ok b)      =  Ok b


mapError :: (a -> e) -> Thing a b c -> Thing e b c
mapError f (Error a)   =  Error (f a)
mapError _ (Fail b)    =  Fail b
mapError _ (Ok c)      =  Ok c


mapFE :: (a -> c) -> Thing a a b -> Thing c c b
mapFE f = mapFail f . mapError f


(<?>) :: String -> Parser t a -> Parser t a
name <?> p = Parser h
  where
    h xs = mapFE (\(ns,ts,us) -> (name:ns,ts,us)) (getParser p xs)



data Parser t a = Parser {
      getParser ::  [t] -> 
          Thing ([String], [t], [t]) 
                ([String], [t], [t]) 
                ([t], a) 
  }




instance Functor' (Parser s) where
  fmap f (Parser g) = Parser (fmap (fmap f) . g)
{- I'm shocked that this doesn't work;
what does it do, invoke a circular definition?
fmap f p = 
      p >>= \x -> 
      pure (f x)
-}

instance Applicative' (Parser s) where
  f <*> x =
     f   >>= \f' ->
     x   >>= \x' ->
     pure (f' x')

        
instance Pointed' (Parser s) where
  pure a = Parser (\xs -> pure (xs, a))

instance Monad' (Parser s) where
  join (Parser f) = Parser h
    where
      h xs = 
          f xs >>= \(o, Parser g) -> 
          g o  
  
instance Semigroup' (Parser s a) where
  Parser f  <|>  Parser g  =  Parser (\xs -> f xs <|> g xs)
  
instance Monoid' (Parser s a) where
  empty = Parser (Fail . (,,) [] [])
  
instance Switch' (Parser s) where
  switch (Parser f) = Parser (\xs -> h xs (f xs))
    where h xs (Ok _)     =  Fail ([], xs, xs)
          h xs (Fail _)   =  Ok (xs, ())
          h _  (Error z)  =  Error z



-- -------------------------

-- succeeds, consuming one 'token', as
--   long as input is not empty
getOne :: Parser s s
getOne = Parser (\xs -> case xs of 
                        (y:ys) -> pure (ys, y);
                        _      -> Fail ([], xs, xs))
  

-- have to preserve the original position
-- in token stream for error reporting, 
-- otherwise this would be a lot simpler  
check :: (a -> Bool) -> Parser s a -> Parser s a
check pred p = Parser h
  where 
    h xs = mapFE (f xs) (getParser parser xs)
    f ys (ns, _, _) = (ns, ys, ys)
    parser =
        p >>= \x -> 
        guard (pred x) >> 
        pure x


satisfy :: (a -> Bool) -> Parser a a
satisfy p = check p getOne
  
  
literal :: Eq a => a -> Parser a a
literal tok = satisfy (== tok)
  
  
-- match both parsers in sequence, and return 
--   the value of the second parser
-- (*>) :: Parser t a -> Parser t b -> Parser t b
l *> r = fmap (flip const) l <*> r 


-- match both parsers in sequence, and return
--   the value of the first parser
-- (<*) :: Parser t a -> Parser t b -> Parser t a
l <* r = fmap const l <*> r


-- match parser 0 or more times
--   couldn't this also be accomplished with a fold?
many :: Parser t a -> Parser t [a]
many p = some p <|> pure []

        
-- match parser 1 or more times
some :: Parser t a -> Parser t [a]
some p = fmap (:) p <*> many p


optional :: Parser t a -> Parser t (Maybe a)
optional p = fmap Just p <|> pure Nothing


sepBy1 :: Parser t a -> Parser t b -> Parser t ([a], [b])
sepBy1 p s = fmap g p <*> (liftA2 f s (sepBy1 p s) <|> pure ([], []))
  where 
    f a (b, c) = (b, a:c)
    g d (e, f) = (d:e, f) -- why are we shadowing f here ???
    

sepBy0 :: Parser t a -> Parser t b -> Parser t ([a], [b])
sepBy0 p s = sepBy1 p s <|> pure ([], [])


end :: Parser t ()
end = switch getOne


not1 :: Parser t b -> Parser t t
not1 p = switch p *> getOne


-- matches if next token is not x, consuming one token
pnot :: Eq t => t -> Parser t t
pnot x = satisfy (/= x)
-- how about:
--  pnot x = not1 (literal x)
--  pnot = not1 . literal


-- matches if next token not in xs, consuming one token
--   not sure if I like this one
pnone :: Eq t => [t] -> Parser t t
pnone xs = satisfy (\x -> not $ elem x xs)
-- how about:
--  pnone xs = not1 (pany $ map literal xs)
--  pnone = not1 . pany . map literal


-- matches all of the tokens in sequence
string :: Eq t => [t] -> Parser t [t]
string = commute . map literal


-- should I add something in here
-- to make it keep track of where
-- it was in the token stream when
-- it started the parser?  (in case
-- it fails, for reporting)
commit :: Parser t a -> Parser t a
commit p = Parser (\xs -> h xs $ getParser p xs)
  where
    h xs (Fail (ts,_,ys))   =  Error (ts,xs,ys)
    h _  x                  =  x
