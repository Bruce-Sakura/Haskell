data Expr
    = Val Int          -- 一个值
    | Add Expr Expr    -- 一个加法表达式

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

-- ghci> 
-- ghci> eval (Add (Val 3) 
-- ghci> eval (Add (Val 3) (Val 3))
-- 6