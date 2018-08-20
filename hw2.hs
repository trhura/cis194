import Log

parseWords :: [String] -> LogMessage
parseWords [] = Unknown ""
parseWords message@(x:y:z:xs) = case x of
    "I" -> LogMessage Info (read y) (unwords $ z:xs)
    "W" -> LogMessage Warning (read y) (unwords $ z:xs)
    "E" -> LogMessage (Error $ read y) (read z) (unwords xs)
    _ -> Unknown $ unwords message

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines
