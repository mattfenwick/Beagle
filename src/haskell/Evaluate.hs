module Evaluate (

) where

import Parser (ASTNode(..), full) -- as Parser
import qualified Data.Map as Map
import Control.Monad (foldM, liftM)


-- --------------------------------------------------------
-- the evaluator

type Environment = Map.Map String LispVal

data LispVal
  = LNumber Float
  | LList [LispVal]
  | LChar Char
  | LBoolean Bool
  | LNull
  | LFunc ([LispVal] -> Either String LispVal)
  | LSpecial (Environment -> [ASTNode] -> Either String (Environment, LispVal))
  
  
instance Show LispVal where
  show (LNumber f)  = show f
  show (LList fs)   = show $ map show fs
  show (LChar c)    = show c
  show (LFunc f)    = "function"
  show (LBoolean b) = show b
  show LNull        = "<nil>"
  show (LSpecial s) = "special form"


applySpecial :: Environment 
  -> (Environment -> [ASTNode] -> Either String (Environment, LispVal)) 
  -> [ASTNode] 
  -> Either String (Environment, LispVal)
applySpecial e s form = s e form

{- -- needs Eq instance or something ... odd
define :: Environment -> [ASTNode] -> Either String (Environment, LispVal)
define e [ASymbol sym, val]
  | Map.lookup sym e == Nothing = eval e val >>=
                                  (\(e1, v) -> let e2 = Map.insert sym v
                                               in return (e2, LNull))
  | otherwise = Left ("symbol " ++ sym ++ " cannot be rebound")
  -}

cons :: [LispVal] -> Either String LispVal
cons [e, LList es] = Right $ LList (e:es)
cons [_, x] = Left ("type error in 'cons': expected list, got " ++ (show x))
cons _ = Left "NumArgsError in 'cons'"

  
applyFunction :: Environment
  -> ([LispVal] -> Either String LispVal)
  -> [ASTNode]
  -> Either String (Environment, LispVal)
applyFunction e f forms = evalForms e forms >>= 
  (\(e1, vals) -> f vals >>=
  (\result -> return (e1, result)))


apply :: Environment -> LispVal -> [ASTNode] -> Either String (Environment, LispVal)
apply e (LFunc f) args = applyFunction e f args -- Right (e, f args)
apply e (LSpecial s) forms = s e forms
apply _ _ _ = Left "hey, you didn't give me a function or special form!"


evalForm :: (Environment, [LispVal]) -> ASTNode -> Either String (Environment, [LispVal])
evalForm (e0, vals) form = eval e0 form >>=
  (\(e1, val) -> return (e1, val : vals))


evalForms :: Environment -> [ASTNode] -> Either String (Environment, [LispVal])
evalForms e fs = foldM evalForm (e, []) fs
  
  
eval :: Environment -> ASTNode -> Either String (Environment, LispVal)
eval e (ANumber f)  = Right (e, LNumber f)
eval e (AChar c)    = Right (e, LChar c)
eval e (ASymbol s)  = (fromMaybe $ Map.lookup s e) >>= (\v -> return (e, v))
  where fromMaybe Nothing = Left ("unbound variable: " ++ s)
        fromMaybe (Just x) = Right x
eval e (AList fs)   = liftM (\(a, b) -> (a, LList $ reverse b)) $ evalForms e fs
eval e (Application op args) = eval e op >>=
  (\(e1, f) -> apply e1 f args)

  
defaultEnv = Map.fromList [("true", LBoolean True), ("false", LBoolean False)]


      
evaluator :: String -> Either String [LispVal]
evaluator str = full str >>= 
  mapM (eval defaultEnv) >>= 
  (return . (Prelude.map snd))
  


