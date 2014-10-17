module Expr where

import Test.QuickCheck
import Data.Char
import Data.Maybe

--Part I
---------------------------------------

--A
data Expr 
    = Num Double
    | Var
    | Add Expr Expr
    | Mul Expr Expr
    | Sin Expr
    | Cos Expr
  deriving (Eq)

-- Some Expr for debuging.
expr1 = Sin (Mul (Num 2) (Var))
expr2 = Mul (Mul (Num 2) (Var)) (Add (Num 5) (Num 3))
expr3 = Mul (Mul (Num 2) (Num 4)) (Add (Num 5) (Num 3))
expr4 = Mul (Add (Num 2) (Num 2)) (Add (Num 3) (Num 3))


--B
-- Returns the expresion as a readable String.
showExpr :: Expr -> String
showExpr (Num x)                       = show x
showExpr (Var)                       = "x"
showExpr (Add a           b)           =        showExpr a ++  "+"  ++ showExpr b
showExpr (Mul a@(Add _ _) b@(Add _ _)) = "(" ++ showExpr a ++ ")*(" ++ showExpr b ++ ")"
showExpr (Mul a@(Add _ _) b)           = "(" ++ showExpr a ++ ")*"  ++ showExpr b
showExpr (Mul a           b@(Add _ _)) =        showExpr a ++  "*(" ++ showExpr b ++ ")"
showExpr (Mul a           b)           =        showExpr a ++  "*"  ++ showExpr b
showExpr (Sin x)                       = "sin(" ++ showExpr x ++ ")"
showExpr (Cos x)                       = "cos(" ++ showExpr x ++ ")"

-- Makes showExpr the default function for showing Expr.
instance Show Expr where
    show = showExpr


-- C
-- Calculates the value of the Expr given a value for Var.
eval :: Expr -> Double -> Double
eval (Num n)   x = n
eval (Var) x = x
eval (Add a b) x = eval a x + eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Sin n)   x = sin (eval n x)
eval (Cos n)   x = cos (eval n x)


-- D
	
-------------------------------------------------------------------------

type Parser a = String -> Maybe (a,String)

-- `number` parses a number
number :: Parser Double
number ('-':s)           = fmap negate' (number s)
number (c:s) | isDigit c = Just (head (reads (c:s) :: [(Double, String)]))
number _                 = Nothing

negate' :: (Double,String) -> (Double,String)
negate' (n,s) = (-n,s)

-- `num` parses a numeric expression
num :: Parser Expr
num s = case number s of
    Just (n,s') -> Just (Num n, s')
    Nothing     -> Nothing

-------------------------------------------------------------------------

-- * an expression is a '+'-chain of terms
-- * a term is a '*'-chain of factors
expr, term :: Parser Expr
expr = chain term   '+' Add
term = chain factor '*' Mul

-- `chain p op f s1` parsers a "chain" of things.
--
--   * The things are parsed by the parser `p`.
--   * The things are separated by the symbol `op`.
--   * The things are combined by the function `f`.
--
-- For example "12+23+1+172" is a chain of numbers, separated by the symbol '+'.
chain :: Parser a -> Char -> (a -> a -> a) -> Parser a
chain p op f s1 =
  case p s1 of
    Just (a,s2) -> case s2 of
                     c:s3 | c == op -> case chain p op f s3 of
                                         Just (b,s4) -> Just (f a b, s4)
                                         Nothing     -> Just (a,s2)
                     _              -> Just (a,s2)
    Nothing     -> Nothing

-- `factor` parses a "factor": either a number or an expression surrounded by
-- parentheses
factor :: Parser Expr
factor ('(':s) =
   case expr s of
      Just (a, ')':s1) -> Just (a, s1)
      _                -> Nothing
factor ('x':rest) = Just (Var, rest)
factor ('s':'i':'n':s) = case factor s of
                             Just (a, s1) -> Just (Sin a, s1)
                             _            -> Nothing
factor ('c':'o':'s':s) = case factor s of
                             Just (a, s1) -> Just (Cos a, s1)
                             _            -> Nothing
factor s = num s

-- `readExpr` reads a string into an expression
readExpr :: String -> Maybe Expr
readExpr s =
  case expr s of
    Just (a,"") -> Just a
    _           -> Nothing

-- Makes sure a Expr is equal to readExpr (show Expr).
-- Cases where associativity makes two different looking Expr equal
-- is concidered equal thanks to function assoc.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = Just (assoc e) == readExpr (show e)

-- Function for generating arbitrary Expr
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [
                (1, do n <- arbitrary
                       return (Num n)),
                (1, do return Var),
                (s, do a <- arbExpr s'
                       b <- arbExpr s'
                       return (Add a b)),
                (s, do a <- arbExpr s'
                       b <- arbExpr s'
                       return (Mul a b)),
                (s, do a <- arbExpr s'
                       return (Sin a)),
                (s, do a <- arbExpr s'
                       return (Cos a))
            ]
    where 
        s' = div s 2

instance Arbitrary Expr where
    arbitrary = sized arbExpr

-- 'formats' an Expr as it would have been returned by readExpr
assoc :: Expr -> Expr
assoc (Add (Add a b) c) = assoc (Add a (Add b c))
assoc (Add a b)         = Add (assoc a) (assoc b)
assoc (Mul (Mul a b) c) = assoc (Mul a (Mul b c))
assoc (Mul a b)         = Mul (assoc a) (assoc b)
assoc (Sin a)           = Sin (assoc a)
assoc (Cos a)           = Cos (assoc a)
assoc a                 = a





