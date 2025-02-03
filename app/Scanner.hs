module Scanner (scanPort, resolve, openSocket', tryConnect, detectService) where

import Network.Socket
import qualified Data.ByteString.Char8 as B
import Control.Exception (try, IOException)

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