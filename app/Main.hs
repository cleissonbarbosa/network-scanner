{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import qualified Data.ByteString.Char8 as B
import Control.Exception (try, IOException)
import Control.Concurrent.Async (mapConcurrently)
import System.IO (writeFile)
import Data.Time (getCurrentTime)

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
    -- Imprime os resultados no console
    mapM_ putStrLn results
    -- Obter data e hora atuais
    time <- getCurrentTime
    let reportHeader = "Scan report generated at: " ++ show time ++ "\n\n"
    -- Gera relatório em arquivo com data e hora
    writeFile "scan_report.txt" (reportHeader ++ unlines results)