module Scanner (scanPort, scanPortWithTimeout, resolve, openSocket', tryConnect, detectService) where

import Network.Socket
import Control.Exception (try, IOException)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Logger (formatScanResult)

-- Escanear uma porta específica e retornar o resultado como String
scanPort :: HostName -> PortNumber -> IO String
scanPort host port = do
    addr <- resolve host port
    sock <- openSocket' addr
    result <- tryConnect sock addr
    res <- case result of
        Just _  -> do
            let service = detectService port
            return $ formatScanResult host port True service
        Nothing -> return $ formatScanResult host port False ""
    close sock
    return res

-- Escanear uma porta com timeout (melhor controle de concorrência)
scanPortWithTimeout :: Int -> HostName -> PortNumber -> IO String
scanPortWithTimeout timeoutSeconds host port = do
    result <- race (threadDelay (timeoutSeconds * 1000000)) (scanPort host port)
    case result of
        Left _  -> return $ formatScanResult host port False "TIMEOUT"
        Right scanResult -> return scanResult

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
detectService 21    = "FTP"
detectService 22    = "SSH"
detectService 23    = "Telnet"
detectService 25    = "SMTP"
detectService 53    = "DNS"
detectService 80    = "HTTP"
detectService 110   = "POP3"
detectService 143   = "IMAP"
detectService 443   = "HTTPS"
detectService 993   = "IMAPS"
detectService 995   = "POP3S"
detectService 3306  = "MySQL"
detectService 3389  = "RDP"
detectService 5432  = "PostgreSQL"
detectService 5900  = "VNC"
detectService 8080  = "HTTP-Alt"
detectService 8443  = "HTTPS-Alt"
detectService 9000  = "SonarQube"
detectService _     = "Unknown Service"