-- Author Salvador Orozco Villalever - A07104218
-- Version: 12/03/2018

module Utilities where

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

-- Function that deletes duplicates in a list
removeListDuplicates :: (Ord a) => [a] -> [a]
removeListDuplicates = map head . group . sort

-- Functions to normalize the text
toLowerStr xs = map toLower xs
dropNonLetters xs = words $ (filter (\x -> x `elem` (' ':['a'..'z']))) $ toLowerStr xs

-- Function to clean words
cleanWords :: [String] -> [String]
cleanWords [w] = dropNonLetters w
cleanWords (w:ws) = (dropNonLetters w) ++ (cleanWords ws)

-- Function to remove common words from the chat, i.e. words that
-- don't provide actual value to the analysis
removeCommonWords :: [String] -> [String] -> [String]
removeCommonWords words commonWords = filter (`notElem` commonWords) words

-- Function to get the day of the week given the number of the day
getWeekDay :: Int -> String
getWeekDay weekDayNumber 
    | (weekDayNumber == 1) = "Monday"
    | (weekDayNumber == 2) = "Tuesday"
    | (weekDayNumber == 3) = "Wednesday"
    | (weekDayNumber == 4) = "Thursday"
    | (weekDayNumber == 5) = "Friday"
    | (weekDayNumber == 6) = "Saturday"
    | otherwise = "Sunday"

-- Function that returns a list of the days of the week
getWeekDaysList :: [String]
getWeekDaysList = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

-- Function that gets the day of the week of a given message
getMessageWeekDay :: WhatsAppMessage -> String
getMessageWeekDay message = getWeekDay weekDayNumber
    where
        (year, month, day) = toGregorian $ (localDay (getMessageTimeStamp message))
        (_, _, weekDayNumber) = toWeekDate $ fromGregorian year month day

-- Function that computes the length of the chat in days.
getChatLength :: [WhatsAppMessage] -> Integer
getChatLength parsedMessages = diffDays lastChatDay firstChatDay
    where
        firstChatMessage = getMessageByIndex parsedMessages 0
        lastChatMessage = getMessageByIndex parsedMessages ((length parsedMessages) - 1)
        firstChatDay = localDay (getMessageTimeStamp firstChatMessage)
        lastChatDay = localDay (getMessageTimeStamp lastChatMessage)
