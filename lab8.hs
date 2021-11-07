import Data.Char (chr)

data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
	  deriving (Eq)

eval :: Expr -> Int
eval (Num x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

showExpr :: Expr -> String
showExpr (Num x) = show x
showExpr (Var x) = show x
showExpr (Add x y) = showExpr x ++ "+" ++ showExpr y
showExpr (Sub x y) = showExpr x ++ "-" ++ showExpr y
showExpr (Mul x y) = "(" ++ showExpr x ++ "*" ++ showExpr y ++ ")"
showExpr (Div x y) = showExpr x ++ "/" ++ showExpr y

instance Show Expr where
    show expr = showExpr expr

deriv :: Expr -> Char -> Expr
deriv (Num x) _ = Num 0
deriv (Var x) y = if x == y then Num 1 else Num 0
deriv (Add x y) var = Add (deriv x var) (deriv y var)
deriv (Sub x y) var = Sub (deriv x var) (deriv y var)
deriv (Mul x y) var = Add (Mul (deriv x var) y) (Mul x (deriv y var))
deriv (Div x y) var = Div (Sub (Mul (deriv x var) y) (Mul x (deriv y var))) (Mul y y)
