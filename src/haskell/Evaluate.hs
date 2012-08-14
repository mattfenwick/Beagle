module Evaluate (

    LispVal (..)

  , evaluator

) where


import qualified Data.Map as Map
import Control.Monad (foldM, liftM)

-- local imports
import MyData (LispVal(..))
import Functions (cons, car, cdr, plus)
import Parser (ASTNode(..), full) -- as Parser
import Environment (Environment(Env), addBinding, getBinding, setBinding)


-- --------------------------------------------------------
-- the evaluator
  
define :: Environment LispVal -> [ASTNode] -> Either String (Environment LispVal, LispVal)
define e [ASymbol key, node] = eval e node >>=
  (\(e1, val) -> addBinding key val e1 >>= 
  (\e2 -> return (e2, LNull)))
  
  
setbang :: Environment LispVal -> [ASTNode] -> Either String (Environment LispVal, LispVal)
setbang e [ASymbol key, node] = eval e node >>=
  (\(e1, val) -> setBinding key val e1 >>=
  (\e2 -> return (e2, LNull)))
  
  
myIf :: LispVal -> Either String Bool
myIf (LBoolean x) = Right x
myIf _ = Left "expected boolean, got ... something else ..."
  
  
cond :: Environment LispVal -> [ASTNode] -> Either String (Environment LispVal, LispVal)
cond e ((AList [p, r]) : forms) = eval e p >>=
  (\(e1, v1) -> myIf v1 >>=
  (\bool -> if bool then (eval e1 r) 
                    else (cond e1 forms)))
cond _ ((AList _):forms) = Left "value error in 'cond': expected two-element list"
cond _ (_:forms) = Left "type error in 'cond': expected list"
cond _ [] = Left "value error in cond: no true condition found"


  
applyFunction :: Environment LispVal
  -> ([LispVal] -> Either String LispVal)
  -> [ASTNode]
  -> Either String (Environment LispVal, LispVal)
applyFunction e f forms = evalForms e forms >>= 
  (\(e1, vals) -> f vals >>=
  (\result -> return (e1, result)))  


apply :: Environment LispVal -> LispVal -> [ASTNode] -> Either String (Environment LispVal, LispVal)
apply e (LFunction f) args = applyFunction e f args
apply e (LSpecial s) forms = s e forms
apply _ _ _ = Left "'apply' needs a function or special form"


evalForm :: (Environment LispVal, [LispVal]) -> ASTNode -> Either String (Environment LispVal, [LispVal])
evalForm (e0, vals) form = eval e0 form >>=
  (\(e1, val) -> return (e1, val : vals))


evalForms :: Environment LispVal -> [ASTNode] -> Either String (Environment LispVal, [LispVal])
evalForms e fs = foldM evalForm (e, []) fs >>=
  (\(e, vals) -> return (e, reverse vals))
  
  
eval :: Environment LispVal -> ASTNode -> Either String (Environment LispVal, LispVal)
eval e (ANumber f)  = Right (e, LNumber f)
eval e (AChar c)    = Right (e, LChar c)
eval e (ASymbol s)  = getBinding s e >>= \v -> return (e, v)
eval e (AList fs)   = evalForms e fs >>= \(e, vals) -> return (e, LList vals)
eval e (Application op args) = eval e op >>= \(e1, f) -> apply e1 f args

  
defaultEnv = Env (Map.fromList [
    ("true",    LBoolean True), 
    ("false",   LBoolean False),
    ("cons",    LFunction cons),
    ("car",     LFunction car),
    ("cdr",     LFunction cdr),
    ("plus",    LFunction plus),
    ("define",  LSpecial define),
    ("setbang", LSpecial setbang),
    ("cond",    LSpecial cond)]) Nothing


      
evaluator :: String -> Either String [(Environment LispVal, LispVal)]
evaluator str = full str >>= 
  mapM (eval defaultEnv)
  


