module WhatsAppMessage where

import Data.Time

data WhatsAppMessage = WhatsAppMessage
    {
        timeStamp :: LocalTime,
        author :: String,
        text :: String
    } deriving (Show)