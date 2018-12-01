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

getDateOfMessage :: WhatsAppMessage -> LocalTime
getDateOfMessage (WhatsAppMessage timeStamp _ _) = timeStamp

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
    -- putStrLn $ show (parsedMessages)
    putStrLn "\n"
    putStr "First message on "
    putStrLn $ show (getDateOfMessage (getMessageByIndex parsedMessages 0))