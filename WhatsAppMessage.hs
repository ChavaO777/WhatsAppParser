-- Author Salvador Orozco Villalever - A07104218
-- Version: 12/03/2018

module WhatsAppMessage where

import Data.Time

-- Data type for each WhatsApp message
data WhatsAppMessage = WhatsAppMessage
    {
        timeStamp :: LocalTime,
        author :: String,
        text :: String
    } deriving (Show)