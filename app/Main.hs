{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import qualified Data.ByteString.Char8 as B
import Control.Exception (try, IOException)
import Control.Concurrent.Async (mapConcurrently)
import System.IO (writeFile)
import Data.Time (getCurrentTime)
import System.Process (readProcess)
import System.Info (os)
import Data.List (isPrefixOf)
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- Escanear uma porta específica e retornar o resultado como String
scanPort :: HostName -> PortNumber -> IO String
scanPort host port = do
    addr <- resolve host port
    sock <- openSocket' addr
    result <- tryConnect sock addr
    res <- case result of
        Just _  -> do
            let service = detectService port
            return $ "Port " ++ show port ++ " is open (" ++ service ++ ")"
        Nothing -> return $ "No connection to port " ++ show port
    close sock
    return res

-- Resolver o endereço do host
resolve :: HostName -> PortNumber -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    return addr

-- Abrir um socket
openSocket' :: AddrInfo -> IO Socket
openSocket' addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- Tentar conectar ao socket
tryConnect :: Socket -> AddrInfo -> IO (Maybe ())
tryConnect sock addr = do
    result <- try (connect sock (addrAddress addr)) :: IO (Either IOException ())
    return $ either (const Nothing) Just result

-- Detectar serviço com base em portas conhecidas
detectService :: PortNumber -> String
detectService 80    = "HTTP"
detectService 443   = "HTTPS"
detectService 22    = "SSH"
detectService 21    = "FTP"
detectService 8080  = "HTTP-Alt"
detectService _     = "Unknown Service"

-- Extrair TTL do output do ping
extractTTL :: String -> Maybe Int
extractTTL output =
    let ttlStrs = [ drop 4 word | word <- words output, "TTL=" `isPrefixOf` word || "ttl=" `isPrefixOf` word ]
    in case ttlStrs of
         (x:_) -> readMaybe (takeWhile isDigit x)
         _     -> Nothing

-- Realiza fingerprinting do SO utilizando o TTL do comando ping, funcionando em Windows e Linux
fingerprintOS :: HostName -> IO String
fingerprintOS host = do
    let (flag, count) = if os == "mingw32" || os == "win32"
                           then ("-n", "1")
                           else ("-c", "1")
    output <- readProcess "ping" [flag, count, host] ""
    case extractTTL output of
        Just ttl -> return $ "OS Fingerprint based on TTL (" ++ show ttl ++ "): " ++ osGuess ttl
        Nothing  -> return "OS Fingerprint: Unable to determine TTL."

-- Adivinha o SO com base no valor TTL
osGuess :: Int -> String
osGuess ttl
    | ttl >= 128 && ttl < 130 = "Windows"
    | ttl == 64               = "Linux/Unix"
    | ttl >= 254              = "Network device or router"
    | otherwise               = "Unknown OS"

-- Função principal
main :: IO ()
main = do
    let host = "127.0.0.1"
    -- Porta específicas e intervalos de portas
    let singlePorts = [80, 443, 8080]
    let rangePorts = [75..85]
    let ports = singlePorts ++ rangePorts
    -- Escaneia de forma concorrente
    results <- mapConcurrently (scanPort host) ports
    -- Obter data e hora atuais
    time <- getCurrentTime
    let reportHeader = "Scan report generated at: " ++ show time ++ "\n\n"
    -- Realiza fingerprinting do SO
    osFingerprint <- fingerprintOS host
    -- Imprime os resultados no console
    mapM_ putStrLn results
    putStrLn osFingerprint
    -- Gera relatório em arquivo com data, hora e fingerprinting do SO
    let fullReport = reportHeader ++ unlines results ++ "\n" ++ osFingerprint
    writeFile "scan_report.txt" fullReport