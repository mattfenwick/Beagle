module Functions (

    cons
  , car
  , cdr
  , plus

) where


import MyData (LispVal(..))


cons :: [LispVal] -> Either String LispVal
cons [e, LList es] = Right $ LList (e:es)
cons [_, x] = Left ("type error in 'cons': expected list, got " ++ (show x))
cons _ = Left "NumArgsError in 'cons'"


car :: [LispVal] -> Either String LispVal
car [LList (e:es)] = Right e
car [LList []] = Left "value error in 'car': can't apply to empty list"
car [_] = Left "type error in 'car': needs Lisp list"
car _ = Left "NumArgsError in 'car'"


cdr :: [LispVal] -> Either String LispVal
cdr [LList (e:es)] = Right $ LList es
cdr [LList []] = Left "value error in 'cdr': can't apply to empty list"
cdr [_] = Left "type error in 'cdr': needs Lisp list"
cdr _ = Left "NumArgsError in 'cdr'" 


plus :: [LispVal] -> Either String LispVal
plus [LNumber x, LNumber y] = Right (LNumber $ x + y)
plus [LNumber _, _] = Left "type error in '+': 2nd arg"
plus [_, _] = Left "type error in '+': 1st arg"
plus _ = Left "NumArgsError in '+'"