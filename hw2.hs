import Log

parseWords :: [String] -> LogMessage
parseWords message@("I":ts:xs) = LogMessage Info (read ts) (unwords xs)
parseWords message@("W":ts:xs) = LogMessage Warning (read ts) (unwords xs)
parseWords message@("E":l:ts:xs)  = LogMessage (Error $ read l) (read ts) (unwords xs)
parseWords others = Unknown $ unwords others

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg@(LogMessage _ _ _) (Leaf) = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node l nmsg@(LogMessage _ nts _) r)
    | ts > nts = Node (insert msg l) nmsg r
    | nts < ts = Node l nmsg (insert msg r)
    | otherwise = Node l nmsg (insert msg r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder r) ++ [msg] ++ (inOrder l)

isSevere :: LogMessage -> Bool
isSevere (Unknown _) = False
isSevere (LogMessage (Error serverity) _ _) = serverity >= 50
isSevere (LogMessage _ _ _) = False

justMessage :: LogMessage -> String
justMessage (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map justMessage $ filter isSevere $ inOrder $ build xs
