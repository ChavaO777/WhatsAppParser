import System.Environment
import Data.Time
import Data.List

-- Data type of a WhatsApp message
data WhatsAppMessage = WhatsAppMessage
    {
        timeStamp :: LocalTime,
        author :: String,
        text :: String
    } deriving (Show)

-- Function that parses the timestamp of a message given its string
parseTimeStamp :: String -> LocalTime
parseTimeStamp message = 
    let
        timeString = takeWhile(/= ']') message
        timeFormatString = "%-m/%-d/%y, %-H:%M:%S %p"
    in
        parseTimeOrError True defaultTimeLocale timeFormatString (drop 1 timeString) :: LocalTime


-- Function that parses the author of a WhatsApp message given its string
parseAuthor :: String -> String
parseAuthor = takeWhile(/= ':') . drop 2 . dropWhile(/= ']')

-- Function that parses the text of a WhatsApp message given its string
parseText :: String -> String
parseText = takeWhile(/= '[') . drop 2 . dropWhile(/= ':') . drop 2 . dropWhile(/= ']')

-- Function that parses a WhatsApp message given its string
parseMessage :: String -> WhatsAppMessage
parseMessage message = WhatsAppMessage 
    {
        timeStamp = parseTimeStamp message,
        author = parseAuthor message,
        text = parseText message
    }

-- Function that extracts the date of a given message
getMessageTimeStamp :: WhatsAppMessage -> LocalTime
getMessageTimeStamp (WhatsAppMessage timeStamp _ _) = timeStamp

-- Function that extract the author of a given message
getMessageAuthor :: WhatsAppMessage -> String
getMessageAuthor (WhatsAppMessage _ author _) = author

-- Function that extracts the author of each message and returns a 
-- list of authors per message
extractMessageAuthors :: [WhatsAppMessage] -> [String]
extractMessageAuthors [] = []
extractMessageAuthors (w:ws) = [(getMessageAuthor w)] ++ (extractMessageAuthors ws)

-- Function that retrieves a message given its index
getMessageByIndex :: [WhatsAppMessage] -> Int -> WhatsAppMessage
getMessageByIndex parsedMessages n = (parsedMessages !! n) 

-- Function that parses the whole chat
parseChat :: [String] -> [WhatsAppMessage]
parseChat lines = 
    let parsedLines = lines
    in map (parseMessage) parsedLines

-- Function that deletes duplicates in a list
removeListDuplicates :: (Ord a) => [a] -> [a]
removeListDuplicates = map head . group . sort

-- Function that computes the amount of messages per author
computeMessageCountPerAuthor :: [String] -> [WhatsAppMessage] -> [(String, Int)]
computeMessageCountPerAuthor [a] parsedMessages = [(a, length (filter (\message -> getMessageAuthor message == a) parsedMessages))]
computeMessageCountPerAuthor (a:as) parsedMessages = [(a, length (filter (\message -> getMessageAuthor message == a) parsedMessages))] ++ (computeMessageCountPerAuthor as parsedMessages)

main :: IO()
main = do
    args <- getArgs
    -- Read the input file: _chat.txt
    fileContent <- readFile (head args)
    let
        -- Extract the lines of the file
        messages = lines fileContent
        -- Parse the messages
        parsedMessages = parseChat messages
        -- Compute the total amount of messages
        totalMessageAmount = length parsedMessages
        -- Get the list of message authors (i.e. participants in the chat)
        messageAuthors = removeListDuplicates (extractMessageAuthors parsedMessages)
        -- Get the list of tuples <author, message count>
        messagesPerAuthor = computeMessageCountPerAuthor messageAuthors parsedMessages
    -- Print the total set of messages
    putStrLn $ show (parsedMessages)
    -- Print the timestamp of the first message
    putStr "\nFirst message on: "
    putStrLn $ show (getMessageTimeStamp (getMessageByIndex parsedMessages 0))
    -- Print the timestamp of the last message
    putStr "\nLast message on: "
    putStrLn $ show (getMessageTimeStamp (getMessageByIndex parsedMessages ((length parsedMessages) - 1)))
    -- Print the total amount of messages
    putStr "\nAmount of messages: "
    putStrLn $ show totalMessageAmount
    -- Print the total amount of chat participants
    putStr "\nTotal chat participants: "
    putStrLn $ show (length messageAuthors)
    -- Print the total amount of messages per participant
    putStr "\nTotal messages per participant: \n\n"
    mapM_ print messagesPerAuthor