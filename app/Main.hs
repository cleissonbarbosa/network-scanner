{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import qualified Data.ByteString.Char8 as B
import Control.Exception (try, IOException)

-- Função para escanear uma porta específica
scanPort :: HostName -> PortNumber -> IO ()
scanPort host port = do
    addr <- resolve host port
    sock <- openSocket' addr
    result <- tryConnect sock addr
    case result of
        Just _  -> putStrLn $ "Port " ++ show port ++ " is open"
        Nothing -> putStrLn $ "No connection to port " ++ show port
    close sock

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

main :: IO ()
main = do
    let host = "127.0.0.1"
    let ports = [80, 443, 8080]
    mapM_ (scanPort host) ports