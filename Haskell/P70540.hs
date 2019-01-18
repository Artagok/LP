data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

-- Evaluation without errors (20 points)
-- Using the Expr type, define a function eval1 :: Expr -> Int that, given an expression, returns its evaluation. You can assume there will never be divisions by zero.
eval1 :: Expr -> Int
eval1 (Val x)   = x
eval1 (Add x y) = eval1 x + eval1 y
eval1 (Sub x y) = eval1 x - eval1 y 
eval1 (Mul x y) = eval1 x * eval1 y 
eval1 (Div x y) = (eval1 x) `div` (eval1 y)

-- Evaluation with possible error (30 points)
-- Using the Expr type, define a function eval2 :: Expr -> Maybe Int that, given an expression, returns its evaluationn as a Just value.
-- In the case that some division by zero occurs, the result must be Nothing. You probably want to use the do notation over the Maybe a monad.
eval2 :: Expr -> Maybe Int
eval2 (Val x)   = Just x
eval2 (Add x y) = do {
    i <- eval2 x;
    j <- eval2 y;
    Just (i + j);
}
eval2 (Sub x y) = do {
    i <- eval2 x;
    j <- eval2 y;
    Just (i - j);
} 
eval2 (Mul x y) = do {
    i <- eval2 x;
    j <- eval2 y;
    Just (i * j);
} 
eval2 (Div x y) = do {
    i <- eval2 x; 
    j <- eval2 y;
    case j of 
        0           -> Nothing
        otherwise   -> Just (i `div` j)
}

-- Evaluation with error report (30 points)
-- Using the Expr type, define a function eval3 :: Expr -> Either String Int that, given an expression, returns its evaluation as Right value. 
-- In the case that some division by zero occurs, the result must be Left "div0". You probably want to use the do notation over the Either a b monad.
eval3 :: Expr -> Either String Int
eval3 (Val x)   = Right x
eval3 (Add x y) = do {
    i <- eval3 x;
    j <- eval3 y;
    Right (i + j)
}
eval3 (Sub x y) = do {
    i <- eval3 x;
    j <- eval3 y;
    Right (i - j)
}
eval3 (Mul x y) = do {
    i <- eval3 x;
    j <- eval3 y;
    Right (i * j)
}
eval3 (Div x y) = do {
    i <- eval3 x;
    j <- eval3 y;
    if (j == 0)
        then Left ("div0");
        else Right (i `div` j);
}