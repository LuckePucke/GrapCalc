module ExprQC where

import Test.QuickCheck
import Expr

-- E

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
