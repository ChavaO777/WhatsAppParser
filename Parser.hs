module Parser where

import WhatsAppMessage

import System.Environment
import Data.Time
import Data.List
import Data.List.Split (splitOn)
import Data.Char
import Data.List (sortBy)
import Data.Function (on)
import Data.Time (fromGregorian)

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