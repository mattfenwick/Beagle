module Environment (

    Environment (Env)
  
  , addBinding
  , getBinding

  , setBinding

  , hasBinding
  , hasOwnBinding
  
)  where


import Prelude hiding (lookup)
import Data.Map (Map, member, lookup, insert, fromList)


data Environment a = Env (Map String a) (Maybe (Environment a))
  deriving (Show)


hasOwnBinding :: String -> Environment a -> Bool
hasOwnBinding key (Env bindings _) = member key bindings


hasBinding :: String -> Environment a -> Bool
hasBinding key this@(Env bindings Nothing) = hasOwnBinding key this
hasBinding key this@(Env bindings (Just parent))
  | hasOwnBinding key this = True
  | otherwise = hasBinding key parent


addBinding :: String -> a -> Environment a -> Either String (Environment a)
addBinding key val this@(Env bindings parent)
  | hasOwnBinding key this = Left ("cannot redefine symbol " ++ key)
  | otherwise = Right $ Env (insert key val bindings) parent
  
  
fromMaybe :: String -> Maybe a -> Either String a
fromMaybe message Nothing = Left message
fromMaybe _ (Just x) = Right x

  
getBinding :: String -> Environment a -> Either String a
getBinding key this@(Env bindings Nothing) = fromMaybe ("cannot find symbol " ++ key) $ lookup key bindings
getBinding key this@(Env bindings (Just parent))
  | hasOwnBinding key this = fromMaybe (error "huh?") $ lookup key bindings
  | otherwise = getBinding key parent
  
  
setBinding :: String -> a -> Environment a -> Either String (Environment a)
setBinding key val this@(Env bindings Nothing)
  | hasOwnBinding key this = Right $ Env (insert key val bindings) Nothing
  | otherwise = Left ("cannot set! undefined symbol " ++ key)
setBinding key val this@(Env bindings (Just parent))
  | hasOwnBinding key this = Right $ Env (insert key val bindings) (Just parent)
  | otherwise = setBinding key val parent >>= \e -> return (Env bindings $ Just e)
  


