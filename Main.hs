import System.Environment
import Data.Time
import Data.List
import Data.List.Split (splitOn)
import Data.Char
import Data.List (sortBy)
import Data.Function (on)
import Data.Time (fromGregorian)
 
import Data.Time.Calendar.WeekDate (toWeekDate)

import WhatsAppMessage
import Parser
import Statistics
import Utilities

main :: IO()
main = do
    args <- getArgs
    -- Read the input file. E.g. "_chat.txt"
    fileContent <- readFile (head args)
    let
        sortBySecondElementInTuple = sortBy (flip compare `on` snd)
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
        -- Get the list of tuples <day of the week, message count>
        messagesPerDay = computeMessageCountPerDay getWeekDaysList parsedMessages
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
    putStr "\nTotal words in the chat: "
    putStrLn $ show (length wordsInChat)
    -- Function to sort by the second element in a tuple
    putStr "\nTop "
    putStr $ show(topWordsLimit)
    putStr " words in the chat:\n"
    putStr $ show (take topWordsLimit (sortBySecondElementInTuple (removeListDuplicates (computeWordCount wordsInChat wordsInChat))))
    putStrLn "\n"
    putStr "\nDistribution of sent messages per day of the week (in decreasing order):\n"
    mapM_ print (sortBySecondElementInTuple messagesPerDay)