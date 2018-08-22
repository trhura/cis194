module ExprT where

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)