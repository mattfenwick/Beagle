module Evaluate (

) where

import Parser (ASTNode(..), full) -- as Parser
import qualified Data.Map as Map


-- --------------------------------------------------------
-- the evaluator

type Environment = Map.Map String LispVal

data LispVal
  = LNumber Float
  | LList [LispVal]
  | LChar Char
  | LBoolean Bool
  | LFunc ([LispVal] -> LispVal)
  | LSpecial (Environment -> [ASTNode] -> LispVal)
  
  
instance Show LispVal where
  show (LNumber f) = show f
  show (LList fs) = show $ map show fs
  show (LChar c) = show c
  show (LFunc f) = "function"
  show (LBoolean b) = show b
  
  
apply :: Environment -> LispVal -> [LispVal] -> Either String (Environment, LispVal)
apply e (LFunc f) args = Right (e, f args)
apply e (LSpecial s) forms = Left "special form evaluation is unimplemented"
apply _ _ _ = Left "hey, you didn't give me a function or special form!"


evalForm :: Either String (Environment, [LispVal]) -> ASTNode -> Either String (Environment, [LispVal])
evalForm base nextForm = base >>= (\(env, evaledForms) -> 
  eval env nextForm >>= (\(nextEnv, evaledForm) -> 
  return (nextEnv, evaledForm : evaledForms)))
  
  
eval :: Environment -> ASTNode -> Either String (Environment, LispVal)
eval e (ANumber f) = Right (e, LNumber f)
eval e (AChar c) = Right (e, LChar c)
eval e (ASymbol s) = (fromMaybe $ Map.lookup s e) >>= (\v -> return (e, v))
  where fromMaybe Nothing = Left ("unbound variable: " ++ s)
        fromMaybe (Just x) = Right x
eval e (AList fs) = foldl evalForm (Right (e, [])) fs >>= 
  (\(newEnv, newForms) -> Right (newEnv, LList $ reverse newForms))
eval e (Application op args) = eval e op >>=
  (\(e1, f) -> foldl evalForm (Right (e1, [])) args >>=
  (\(newEnv, newForms) -> apply newEnv f $ reverse newForms))
  
  
defaultEnv = Map.fromList [("true", LBoolean True), ("false", LBoolean False)]


      
evaluator :: String -> Either String [LispVal]
evaluator str = full str >>= 
  mapM (eval defaultEnv) >>= 
  (return . (Prelude.map snd))
  


