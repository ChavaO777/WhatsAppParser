import System.Environment
import Data.Time
import Data.List
import Data.List.Split (splitOn)
import Data.Char
import Data.List (sortBy)
import Data.Function (on)
import Data.Time (fromGregorian)
 
import Data.Time.Calendar.WeekDate (toWeekDate)

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

-- Function that extracts the text of a given message
getMessageText :: WhatsAppMessage -> String
getMessageText (WhatsAppMessage _ _ text) = text

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

computeWordsInChat :: [WhatsAppMessage] -> [String]
computeWordsInChat [w] = splitOn " " (getMessageText w)
computeWordsInChat (w:ws) = (splitOn " " (getMessageText w)) ++ (computeWordsInChat ws)

computeWordCount :: [String] -> [String] -> [(String, Int)]
computeWordCount [w] wordsList = [(w, length (filter (\word -> word == w) wordsList))]
computeWordCount (w:ws) wordsList = [(w, length (filter (\word -> word == w) wordsList))] ++ computeWordCount ws wordsList

toLowerStr xs = map toLower xs
dropNonLetters xs = words $ (filter (\x -> x `elem` (' ':['a'..'z']))) $ toLowerStr xs

cleanWords :: [String] -> [String]
cleanWords [w] = dropNonLetters w
cleanWords (w:ws) = (dropNonLetters w) ++ (cleanWords ws)

removeCommonWords :: [String] -> [String] -> [String]
removeCommonWords words commonWords = filter (`notElem` commonWords) words

getWeekDay :: Int -> String
getWeekDay weekDayNumber 
    | (weekDayNumber == 1) = "Monday"
    | (weekDayNumber == 2) = "Tuesday"
    | (weekDayNumber == 3) = "Wednesday"
    | (weekDayNumber == 4) = "Thursday"
    | (weekDayNumber == 5) = "Friday"
    | (weekDayNumber == 6) = "Saturday"
    | otherwise = "Sunday"

getMessageWeekDay :: WhatsAppMessage -> String
getMessageWeekDay message = getWeekDay weekDayNumber
    where
        (year, month, day) = toGregorian $ (localDay (getMessageTimeStamp message))
        (_, _, weekDayNumber) = toWeekDate $ fromGregorian year month day

main :: IO()
main = do
    args <- getArgs
    -- Read the input file. E.g. "_chat.txt"
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
        -- Get the list of all words in all messages
        commonWords = ["entonces", "mas", "f", "ok", "pues", "oye", "mas", "nada", "este", "pero", "sale", "asi", "que", "de", "y", "el", "si", "la", "no", "es", "ya", "me", "a", "para", "lo", "un", "una", "unos", "unas", "eso", "por", "algo", "se", "esta", "esa", "esto", "eso", "estas", "esas", "estos", "esos", "en", "como", "o", "las", "le", "los", "al", "te", "ese", "con", "del", "tu", "yo", "tan", "hay"]
        wordsInChat = removeCommonWords (cleanWords (computeWordsInChat parsedMessages)) commonWords
        topWordsLimit = 25
        
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
    -- Print the word count in the chat.
    putStr "Total words in the chat: "
    putStrLn $ show (length wordsInChat)
    -- Function to sort by the second element in a tuple
    let sortBySecondElementInTuple = sortBy (flip compare `on` snd)
    putStr "\nTop "
    putStr $ show(topWordsLimit)
    putStr " words in the chat:\n"
    putStr $ show (take topWordsLimit (sortBySecondElementInTuple (removeListDuplicates (computeWordCount wordsInChat wordsInChat))))
    putStrLn "\n"