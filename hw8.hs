import          Employee
import          Data.Tree
import          Data.List
import          Debug.Trace

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
  | fun1 > fun2 = gl1
  | otherwise = gl2

funs :: GuestList -> Integer
funs (GL _ f) = f

employees :: GuestList -> [Employee]
employees (GL emps _) = emps

depthFold :: b -> (a -> b -> b) -> Tree a -> b
depthFold b fn (Node { rootLabel = emp, subForest = []}) = fn emp b
depthFold b fn (Node { rootLabel = emp, subForest = (x:xs)}) = depthFold (depthFold b fn x) fn (Node { rootLabel=emp , subForest=xs})

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp@(Emp _ fun) guestLists = (bossGL <> (GL withoutEmps withoutFun), (GL withEmps withFun))
    where bossGL = (GL [emp] fun)
          withGLs = map fst guestLists
          withFun = sum $ map funs withGLs
          withEmps = concat $ map employees withGLs
          withoutGLs = map snd guestLists
          withoutFun = sum $ map funs withoutGLs
          withoutEmps = concat $ map employees withoutGLs

emptyGL :: GuestList
emptyGL = GL [] 0

breadthFold :: (Show a) => b -> (a -> [b] -> b) -> Tree a -> b
breadthFold b fn (Node { rootLabel = emp, subForest = []}) = fn emp [b]
breadthFold b fn (Node { rootLabel = emp, subForest = xs}) = fn emp (map (breadthFold b fn) xs)

-- maxFun :: Tree Employee -> GuestList
maxFun tree = max with without
                where (with, without) = breadthFold (emptyGL, emptyGL) nextLevel tree


readEmployees :: String -> Tree Employee
readEmployees = read

main = do
    contents <- readFile "company.txt"
    let employees = read contents :: Tree Employee
    let (GL emps fun) = maxFun employees
    putStrLn ("Total fun: " ++ show fun)
    mapM (putStrLn . empName) (sortOn empName emps)
