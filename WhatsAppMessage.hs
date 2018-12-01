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

getMessageByIndex :: [WhatsAppMessage] -> Int -> WhatsAppMessage
getMessageByIndex parsedMessages n = (parsedMessages !! n) 

parseChat :: [String] -> [WhatsAppMessage]
parseChat lines = 
    let parsedLines = lines
    in map (parseMessage) parsedLines

main :: IO()
main = do
    args <- getArgs
    fileContent <- readFile (head args)
    let
        messages = lines fileContent
        parsedMessages = parseChat messages
        totalMessageAmount = length parsedMessages
        salvadorMessages = filter (\message -> getMessageAuthor message == "Salvador" ) parsedMessages
        guillermoMessages = filter (\message -> getMessageAuthor message == "Guillermo Garduno Garcia" ) parsedMessages
    putStrLn $ show (parsedMessages)
    putStr "Amount of messages: "
    putStrLn $ show totalMessageAmount
    putStr "Amount of messages by Salvador: "
    putStrLn $ show (length salvadorMessages)
    putStr "Amount of messages by Guillermo Garduno Garcia: "
    putStrLn $ show (length guillermoMessages)
    putStr "First message on: "
    putStrLn $ show (getMessageDate (getMessageByIndex parsedMessages 0))