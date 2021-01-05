module Chapter11.HuttonsRazor where

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

a1 = Add (Lit 9001) (Lit 1)

a2 = Add a1 (Lit 20001)

a3 = Add (Lit 1) a2
