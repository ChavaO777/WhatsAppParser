module Statistics where

import WhatsAppMessage
import Parser
import Utilities

import System.Environment
import Data.Time
import Data.List
import Data.List.Split (splitOn)
import Data.Char
import Data.List (sortBy)
import Data.Function (on)
import Data.Time (fromGregorian)

-- Function that computes the amount of messages per author
computeMessageCountPerAuthor :: [String] -> [WhatsAppMessage] -> [(String, Int)]
computeMessageCountPerAuthor [a] parsedMessages = [(a, length (filter (\message -> getMessageAuthor message == a) parsedMessages))]
computeMessageCountPerAuthor (a:as) parsedMessages = [(a, length (filter (\message -> getMessageAuthor message == a) parsedMessages))] ++ (computeMessageCountPerAuthor as parsedMessages)

computeMessageCountPerDay :: [String] -> [WhatsAppMessage] -> [(String, Int)]
computeMessageCountPerDay [wd] parsedMessages = [(wd, length (filter (\message -> getMessageWeekDay message == wd) parsedMessages))]
computeMessageCountPerDay (wd:wds) parsedMessages = [(wd, length (filter (\message -> getMessageWeekDay message == wd) parsedMessages))] ++ (computeMessageCountPerDay wds parsedMessages)

computeWordsInChat :: [WhatsAppMessage] -> [String]
computeWordsInChat [w] = splitOn " " (getMessageText w)
computeWordsInChat (w:ws) = (splitOn " " (getMessageText w)) ++ (computeWordsInChat ws)

computeWordCount :: [String] -> [String] -> [(String, Int)]
computeWordCount [w] wordsList = [(w, length (filter (\word -> word == w) wordsList))]
computeWordCount (w:ws) wordsList = [(w, length (filter (\word -> word == w) wordsList))] ++ computeWordCount ws wordsList
