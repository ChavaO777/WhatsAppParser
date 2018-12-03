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
        -- To sort by the second element in a tuple
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
        -- Write a list of common words that are not that important
        commonWords = ["claro", "descansa", "s", "manana", "da", "suena", "tener", "uno", "mano", "mes", "minutos", "sido", "simplemente", "uf", "todos", "casi", "otro", "ir", "solamente", "hacia", "luego", "pienso", "sera", "queria", "bastante", "gusto", "hora", "sabes", "acerca", "hola", "podria", "aunque", "hacerlo", "muchos", "uffffff", "puedo", "estar", "serio", "tienes", "bueno", "nunca", "bien", "parece", "nuevo", "seria", "cual", "aun", "desde", "menos", "ha", "hace", "jajajajajaj", "su", "tenia", "exactamente", "problema", "puede", "soy", "ayer", "hubiera", "quieres", "tres", "buen", "fuera", "saber", "tantas", "todas", "final", "proyecto", "puedes", "quien", "lugar", "otra", "has", "ufffffff", "manos", "rato", "vas", "haces", "semana", "crei", "digo", "ejemplo", "paso", "rapido", "entre", "muchisimo", "parte", "pensar", "manera", "siento", "ahorita", "ay", "espero", "igual", "horas", "nuevo", "donde", "jajajajajajaja", "exacto", "muchas", "excelente", "mismo", "pensando", "iba", "mejor", "verdad", "ganas", "momento", "vi", "ademas", "decir", "cierto", "padre", "repente", "hecho", "tiene", "toda", "aqui", "extrano", "alguna", "e", "embargo", "persona", "veces", "antes", "idea", "pense", "ni", "ah", "hoy", "siempre", "tiempo", "estaba", "todo", "gracias", "jaja", "mientras", "tambien", "jajaja", "jajajaja", "jajajajaj", "mando", "jajajajaja", "mi", "mis", "sin", "nos", "muy", "he", "fue", "mucho", "voy", "entonces", "uffff", "tus", "ti", "mas", "f", "ok", "pues", "oye", "mas", "nada", "este", "pero", "sale", "asi", "que", "de", "y", "el", "si", "ufffff", "hacer", "ver", "cuando", "creo", "dia", "estoy", "poco", "engo", "despues", "vez", "ser", "solo", "hacer", "la", "no", "es", "ya", "me", "a", "para", "lo", "un", "una", "unos", "unas", "eso", "por", "algo", "se", "esta", "esa", "esto", "eso", "estas", "esas", "estos", "esos", "en", "como", "o", "las", "le", "los", "al", "te", "ese", "con", "del", "tu", "tampoco", "buenos", "personas", "entiendo", "mal", "yo", "tan", "hay", "tengo", "porque", "sobre", "son", "jajajajajaja", "eres", "orale", "tal", "rale", "tanto", "va", "era", "cosas", "sea", "ahi", "jajajaj", "etc", "ahora", "favor", "ufff", "dias", "dos", "cada", "hasta", "habia"]
        -- Get the list of all words in all messages
        wordsInChat = removeCommonWords (cleanWords (retrieveWordsInChat parsedMessages)) commonWords
        topWordsLimit = 50
    -- putStrLn $ show parsedMessages
    -- Print the timestamp of the first message
    putStr "\nFirst message on: "
    putStrLn $ show (getMessageTimeStamp (getMessageByIndex parsedMessages 0))
    -- Print the timestamp of the last message
    putStr "\nLast message on: "
    putStrLn $ show (getMessageTimeStamp (getMessageByIndex parsedMessages ((length parsedMessages) - 1)))
    -- Print the amount of days the chat has been going on
    putStr "\nChat length: "
    putStr $ show (getChatLength parsedMessages)
    putStr " days\n"
    -- Print the total amount of messages
    putStr "\nAmount of messages: "
    putStrLn $ show totalMessageAmount
    -- Print the average amount of messages per day
    putStr "\nAverage amount of messages per day: "
    putStrLn $ show (totalMessageAmount `div` (fromIntegral (getChatLength parsedMessages)))
    -- Print the total amount of chat participants
    putStr "\nTotal chat participants: "
    putStrLn $ show (length messageAuthors)
    -- Print the total amount of messages per participant
    putStr "\nTotal messages per participant: \n\n"
    mapM_ print messagesPerAuthor
    -- Print the word count in the chat.
    putStr "\nTotal words in the chat: "
    putStrLn $ show (length wordsInChat)
    -- Print the <topWordsLimit> most common words in the chat
    putStr "\nTop "
    putStr $ show(topWordsLimit)
    putStr " words in the chat:\n"
    putStr $ show (take topWordsLimit (sortBySecondElementInTuple (removeListDuplicates (computeWordCount wordsInChat wordsInChat))))
    putStrLn "\n"
    -- Print the amount of messages sent per day of the week
    putStr "\nDistribution of sent messages per day of the week (in decreasing order):\n"
    mapM_ print (sortBySecondElementInTuple messagesPerDay)