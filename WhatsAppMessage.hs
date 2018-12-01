import System.Environment
import Data.Time
import Data.List

data WhatsAppMessage = WhatsAppMessage
    {
        timeStamp :: LocalTime,
        author :: String,
        text :: String
    } deriving (Show)

parseTimeStamp :: String -> LocalTime
parseTimeStamp message = 
    let
        timeString = takeWhile(/= ']') message
        timeFormatString = "%-m/%-d/%y, %-H:%M:%S %p"
    in
        parseTimeOrError True defaultTimeLocale timeFormatString (drop 1 timeString) :: LocalTime


parseAuthor :: String -> String
parseAuthor = takeWhile(/= ':') . drop 2 . dropWhile(/= ']')

parseText :: String -> String
parseText = takeWhile(/= '[') . drop 2 . dropWhile(/= ':') . drop 2 . dropWhile(/= ']')

parseMessage :: String -> WhatsAppMessage
parseMessage message = WhatsAppMessage 
    {
        timeStamp = parseTimeStamp message,
        author = parseAuthor message,
        text = parseText message
    }

getMessageDate :: WhatsAppMessage -> LocalTime
getMessageDate (WhatsAppMessage timeStamp _ _) = timeStamp

getMessageAuthor :: WhatsAppMessage -> String
getMessageAuthor (WhatsAppMessage _ author _) = author

extractMessageAuthors :: [WhatsAppMessage] -> [String]
extractMessageAuthors [] = []
extractMessageAuthors (w:ws) = [(getMessageAuthor w)] ++ (extractMessageAuthors ws)

getMessageByIndex :: [WhatsAppMessage] -> Int -> WhatsAppMessage
getMessageByIndex parsedMessages n = (parsedMessages !! n) 

parseChat :: [String] -> [WhatsAppMessage]
parseChat lines = 
    let parsedLines = lines
    in map (parseMessage) parsedLines

-- Function that deletes duplicates in a list
removeListDuplicates :: (Ord a) => [a] -> [a]
removeListDuplicates = map head . group . sort

computeMessageCountPerAuthor :: [String] -> [WhatsAppMessage] -> [(String, Int)]
computeMessageCountPerAuthor [a] parsedMessages = [(a, length (filter (\message -> getMessageAuthor message == a) parsedMessages))]
computeMessageCountPerAuthor (a:as) parsedMessages = [(a, length (filter (\message -> getMessageAuthor message == a) parsedMessages))] ++ (computeMessageCountPerAuthor as parsedMessages)

main :: IO()
main = do
    args <- getArgs
    fileContent <- readFile (head args)
    let
        messages = lines fileContent
        parsedMessages = parseChat messages
        totalMessageAmount = length parsedMessages
        messageAuthors = removeListDuplicates (extractMessageAuthors parsedMessages)
        messagesPerAuthor = computeMessageCountPerAuthor messageAuthors parsedMessages
    putStrLn $ show (parsedMessages)
    putStr "\nFirst message on: "
    putStrLn $ show (getMessageDate (getMessageByIndex parsedMessages 0))
    putStr "\nAmount of messages: "
    putStrLn $ show totalMessageAmount
    -- putStr "Amount of messages by Salvador: "
    -- putStrLn $ show (length salvadorMessages)
    -- putStr "Amount of messages by Guillermo Garduno Garcia: "
    -- putStrLn $ show (length guillermoMessages)
    
    -- putStrLn $ show (extractMessageAuthors parsedMessages)
    putStr "\nTotal chat participants: "
    putStrLn $ show (length messageAuthors)
    putStr "\nTotal messages per participant: \n\n"
    mapM_ print messagesPerAuthor