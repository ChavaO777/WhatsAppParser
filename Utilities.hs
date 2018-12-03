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
import Statistics

-- Function that deletes duplicates in a list
removeListDuplicates :: (Ord a) => [a] -> [a]
removeListDuplicates = map head . group . sort

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
