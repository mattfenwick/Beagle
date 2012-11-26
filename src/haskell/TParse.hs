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

) where

import Classes
import Instances () -- what does this do?
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)


data Thing a b c
    = Error a
    | Fail  b
    | Ok    c
  deriving (Show, Eq, Ord)


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
name <?> p = Parser h name
  where
    h xs = mapFE (\(_,ts) -> (name,ts)) (getParser p xs)



data Parser t a = Parser {
        getParser :: ( [t] -> Thing (String, [t]) (String, [t]) ([t], a) )
      , name :: String
    }


-- runParser :: Parser t a -> [t] -> Thing (String, [t]) (String, [t]) ([t], a)
-- runParser p xs = 
    


instance Functor' (Thing a b) where
  fmap f (Ok x)     =  Ok (f x)
  fmap _ (Fail y)   =  Fail y
  fmap _ (Error z)  =  Error z

instance Pointed' (Thing a b) where
  pure = Ok

instance Applicative' (Thing a b) where
  Ok f     <*>   x    =  fmap f x
  Fail y   <*>   _    =  Fail y
  Error z  <*>   _    =  Error z

instance Monad' (Thing a b) where
  join (Ok (Ok x))  =  Ok x
  join (Ok q)       =  q
  join (Fail y)     =  Fail y
  join (Error z)    =  Error z

instance Semigroup' (Thing a b c) where
  Ok x      <|>  _   =  Ok x
  Error z   <|>  _   =  Error z
  Fail _    <|>  r   =  r

instance Foldable' (Thing a b) where
  foldr f base (Ok x)  =  f x base
  foldr _ base   _     =  base

instance Traversable' (Thing a b) where
  commute (Ok    x)  =  fmap Ok x
  commute (Fail  y)  =  pure (Fail y)
  commute (Error z)  =  pure (Error z)




instance Functor' (Parser s) where
  fmap f (Parser g _) = Parser (fmap (fmap f) . g) ""
--  fmap f (Parser g) = Parser (fmap (fmap f) . g)
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
  pure a = Parser (\xs -> pure (xs, a)) ""  

instance Monad' (Parser s) where
  join (Parser f _) = Parser h ""
    where
      h xs = 
          f xs >>= \(o, Parser g _) -> 
          g o  
  
instance Semigroup' (Parser s a) where
  Parser f _  <|>  Parser g _  =  Parser (\xs -> f xs <|> g xs) ""
  
instance Monoid' (Parser s a) where
  empty = Parser (Fail . (,) "empty") ""
  
instance Switch' (Parser s) where
  switch (Parser f _) = Parser h ""
--  switch (Parser f) = Parser h
    where h xs = case (f xs) of
                      (Ok _)     ->  Fail ("switched", xs)
                      (Fail _)   ->  Ok (xs, ())
                      (Error z)  ->  Error z

-- fmap (const (xs, ())) $ switch (f xs)


-- -------------------------

-- succeeds, consuming one 'token', as
--   long as input is not empty
getOne :: Parser s s
getOne = Parser (\xs -> case xs of 
                        (y:ys) -> pure (ys, y);
                        _      -> Fail ("getOne", xs))
                ""
  
  
check :: (a -> Bool) -> Parser s a -> Parser s a
check f p = p >>= \x -> 
  guard (f x) >> 
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


commit :: Parser t a -> Parser t a
commit p = Parser h ""
  where
    h xs = case (getParser p xs) of
                (Ok x)     -> Ok x;
                (Fail y)   -> Error y;
                (Error z)  -> Error z;


setName :: String -> Parser t a -> Parser t a
setName n (Parser f _)  =  Parser f n
