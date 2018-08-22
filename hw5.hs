{-# LANGUAGE InstanceSigs #-}

import ExprT
import Parser

-- eval :: ExprT -> Integer
-- eval (Lit x) = x
-- eval (Add expr1 expr2) = eval expr1 + eval expr2
-- eval (Mul expr1 expr2) = eval expr1 * eval expr2

-- evalStr :: String -> Maybe Integer
-- evalStr s = case parsed of
--         Nothing -> Nothing
--         Just exp -> Just $ eval exp
--     where parsed = parseExp Lit Add Mul s

class Expr a where
    lit :: Integer -> a
    mul :: a -> a ->  a
    add :: a -> a ->  a

instance Expr ExprT where
    lit a = Lit a
    mul a b = Mul a b
    add a b  = Add a b

instance Expr Integer where
    lit =  id
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit = (> 0)
    add a b = a || b
    mul a b = a && b

instance Expr MinMax where
    lit a = MinMax a
    add a b = if (a > b) then a else b
    mul a b = if (b > a) then a else b

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


