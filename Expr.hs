module Expr where

import Test.QuickCheck
import Data.Char

--Part I
---------------------------------------

--A
data Expr 
    = Num Double
    | Var Name
    | Add Expr Expr
    | Mul Expr Expr
    | Fnk Name Expr

type Name = String

expr1 = Fnk "sin" (Mul (Num 2) (Var "x"))
expr2 = Mul (Mul (Num 2) (Var "x")) (Add (Num 5) (Num 3))
expr3 = Mul (Mul (Num 2) (Num 4)) (Add (Num 5) (Num 3))
expr4 = Mul (Add (Num 2) (Num 2)) (Add (Num 3) (Num 3))

-- (2+2) * (3+3)

--B
showExpr :: Expr -> String
showExpr (Num x)                       = show x
showExpr (Var x)                       = x
showExpr (Add a           b)           =        showExpr a ++  "+"  ++ showExpr b
showExpr (Mul a@(Add _ _) b@(Add _ _)) = "(" ++ showExpr a ++ ")*(" ++ showExpr b ++ ")"
showExpr (Mul a@(Add _ _) b)           = "(" ++ showExpr a ++ ")*"  ++ showExpr b
showExpr (Mul a           b@(Add _ _)) =        showExpr a ++  "*(" ++ showExpr b ++ ")"
showExpr (Mul a           b)           =        showExpr a ++  "*"  ++ showExpr b
showExpr (Fnk t           x)           = t ++                   "(" ++ showExpr x ++ ")"

instance Show Expr where
    show = showExpr

-- C
eval :: Expr -> Double -> Double
eval (Num n)   x = n
eval (Var "x") x = x
eval (Add a b) x = eval a x + eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Fnk t n) x
    | t == "sin" = sin (eval n x)
    | t == "cos" = cos (eval n x)

-- D
readExpr' :: String -> Expr
readExpr' ('x':s) = Var "x"
readExpr' ('(':s) | y == ""   = readExpr' x
                  | yc == '+' = Add (readExpr' x) (readExpr' ys)
                  | yc == '*' = Mul (readExpr' x) (readExpr' ys)
                  | take 3 y == "sin" = Fnk "sin" (readExpr' (drop 3 y))
                  | take 3 y == "cos" = Fnk "cos" (readExpr' (drop 3 y))
    where
        brkt      = bracket s "" 0
        x         = fst brkt
        y@(yc:ys) = snd brkt	
readExpr' s | b == ""   = Num (strToDouble a)
			| bc == '+' = Add (Num (strToDouble a)) (readExpr' bs)
			| bc == '*' = Mul (Num (strToDouble a)) (readExpr' bs)
			| bc == '+' = Add (Num (strToDouble a)) (readExpr' bs)
			| take 3 b == "sin" = Fnk "sin" (readExpr' (drop 3 b))
			| take 3 b == "cos" = Fnk "cos" (readExpr' (drop 3 b))
    where
        num       = getNum s ""
        a         = fst num
        b@(bc:bs) = snd num

bracket :: String -> String -> Int -> (String, String)
bracket ""      o n = (reverse o, "")
bracket (')':s) o 0 = (reverse o, s)
bracket (')':s) o n = bracket s (')':o) (n-1)
bracket ('(':s) o n = bracket s ('(':o) (n-1)
bracket (c:s)   o n = bracket s (c:o)   n

getNum :: String -> String -> (String, String)
getNum ""      o = (reverse o, "")
getNum (c:s)   o | elem c ['.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
                   = getNum s (c:o)
                 | otherwise = (reverse o, s)

test :: String -> Char
test (c:s) = c

strToDouble :: String -> Double
strToDouble s = (std (reverse a) 0) + (decimals (reverse b) 0 0)
    where
        a = fst (sep s "")
        b = snd (sep s "")

std :: String -> Double -> Double
std ""    x = 0
std (c:s) x = (mul n (10 ** x)) + (std s (x+1))
    where
        n = digitToInt c

decimals :: String -> Double -> Double -> Double
decimals ""    x y = y / (10 ** x)
decimals (c:s) x y = (decimals s (x+1) ((mul n (10 ** x)) + y))
    where
        n = digitToInt c

mul :: Int -> Double -> Double
mul 0 x = 0
mul n x = x + (mul (n-1) x)

sep :: String -> String -> (String, String)
sep ('.':xs) y = (reverse y, xs)
sep (x:xs)   y = sep xs (x:y)






