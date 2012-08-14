module MyData (

    LispVal(..)

)

where

import Environment (Environment)
import Parser (ASTNode)


data LispVal
  = LNumber Float
  | LList [LispVal]
  | LChar Char
  | LBoolean Bool
  | LNull
  | LDatum String LispVal
  | LFunction ([LispVal] -> Either String LispVal)
  | LSpecial (Environment LispVal -> [ASTNode] -> Either String (Environment LispVal, LispVal))
  deriving (Show)
  
  
instance Show (a -> b) where
  show f = "\"function\""
  
  
