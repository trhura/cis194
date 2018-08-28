import          Employee
import          Data.Tree

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
  | fun1 > fun2 = gl1
  | otherwise = gl2

treeFold :: b -> (b -> a -> b) -> Tree a -> b
treeFold b fn (Node { rootLabel = emp, subForest = []}) = fn b emp
treeFold b fn (Node { rootLabel = emp, subForest = (x:xs)}) = treeFold (treeFold b fn x) fn (Node { rootLabel=emp , subForest=xs})
