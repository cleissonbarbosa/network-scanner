module Logger (LogLevel(..), logMessage, formatScanResult) where

import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import Network.Socket (PortNumber)

-- Logging levels
data LogLevel = INFO | DEBUG | ERROR deriving (Show, Eq)

-- Log a message with timestamp and level
logMessage :: LogLevel -> String -> IO ()
logMessage level message = do
    time <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" time
    putStrLn $ "[" ++ timestamp ++ "] [" ++ show level ++ "] " ++ message

-- Format scan result for better readability
formatScanResult :: String -> PortNumber -> Bool -> String -> String
formatScanResult host port isOpen service = 
    if isOpen 
        then "✓ " ++ host ++ ":" ++ show port ++ " - " ++ service ++ " (OPEN)"
        else "✗ " ++ host ++ ":" ++ show port ++ " - Connection failed (CLOSED)"