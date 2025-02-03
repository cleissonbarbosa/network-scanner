module OSFingerprint (fingerprintOS) where

import Network.Socket (HostName)
import System.Process (readProcess)
import System.Info (os)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.List (isPrefixOf)

extractTTL :: String -> Maybe Int
extractTTL output =
    let ttlStrs = [ drop 4 word | word <- words output, "TTL=" `isPrefixOf` word || "ttl=" `isPrefixOf` word ]
    in case ttlStrs of
         (x:_) -> readMaybe (takeWhile isDigit x)
         _     -> Nothing

fingerprintOS :: HostName -> IO String
fingerprintOS host = do
    let (flag, count) = if os == "mingw32" || os == "win32"
                           then ("-n", "1")
                           else ("-c", "1")
    output <- readProcess "ping" [flag, count, host] ""
    case extractTTL output of
        Just ttl -> return $ "OS Fingerprint based on TTL (" ++ show ttl ++ "): " ++ osGuess ttl
        Nothing  -> return "OS Fingerprint: Unable to determine TTL."

osGuess :: Int -> String
osGuess ttl
    | ttl >= 128 && ttl < 130 = "Windows"
    | ttl == 64               = "Linux/Unix"
    | ttl >= 254              = "Network device or router"
    | otherwise               = "Unknown OS"