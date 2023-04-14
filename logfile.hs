data MessageType = Info
                 | Warning
                 | Error Int 
   deriving (Show,Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
   deriving (Show,Eq)

testString :: String
testString = unlines [
    "I 100 just an info",
    "W 20 just a warning",
    "E 45 150 an error!",
    "E 60 10 another error",
    "I 200 wow what an info",
    "blah blah blah",
    "E 55 250 yet another error"
  ]

testLogMessage :: [LogMessage]
testLogMessage = [
      LogMessage Info 100 "just an info",
      LogMessage Warning 20 "just a warning",
      LogMessage (Error 45) 150 "an error",
      LogMessage Info 200 "wow what an info",
      LogMessage (Error 60) 10 "another error",
      Unknown "blah blah blah",
      LogMessage (Error 55) 250 "yet another error" 
 ]

parseLog :: String -> [LogMessage]
parseLog input = parseLines (lines input)
 where
  parseLines :: [String] -> [LogMessage]
  parseLines [] = []
  parseLines (x:xs) = parseSingleLine x : parseLines xs
  parseSingleLine :: String -> LogMessage
  parseSingleLine list = case (words list) of 
   "I" : timestamp :zs -> LogMessage Info (read timestamp) (unwords zs)
   "W" : timestamp :zs -> LogMessage Warning (read timestamp) (unwords zs)
   "E" :severity: timestamp :zs -> LogMessage (Error (read severity)) (read timestamp) (unwords zs)
   _ -> Unknown list

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
    deriving (Show,Eq)

insert :: LogMessage -> MessageTree ->MessageTree
insert (Unknown _) tree = tree
insert log@(LogMessage _ ts _) Leaf = Node Leaf log Leaf
insert log@(LogMessage _ ts1 _) (Node left x@(LogMessage _ ts2 _) right)
    | ts1< ts2 = Node (insert log left) x right
    | otherwise = Node left x (insert log right) 

buildTree :: [LogMessage] -> MessageTree
buildTree [] = Leaf
buildTree (log:logs) = insert log (buildTree logs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = inOrder left ++ [log] ++ inOrder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = let tree= buildTree logs
                         sorted = inOrder tree
                         filterlogMessage = filterlogs sorted
                      in logsToString filterlogMessage
    where 
        filterlogs :: [LogMessage] -> [LogMessage]
        filterlogs []= []
        filterlogs (log :logs) 
            | isImportantLog log = log : filterlogs logs
            | otherwise = filterlogs logs
        isImportantLog :: LogMessage -> Bool
        isImportantLog (LogMessage (Error severity) _ _)
            | severity >50 = True
        isImportantLog _ = False
        logsToString :: [LogMessage]-> [String]
        logsToString [] =[]
        logsToString (log:logs) = logToString log : logsToString logs
        logToString :: LogMessage -> String
        logToString (LogMessage _ _ msg) = msg
        logToString (Unknown msg) = msg