{-# LANGUAGE ScopedTypeVariables #-}
module NetworkDiscovery (
    generateNetworkHosts,
    pingHost,
    discoverActiveHosts
) where

import Network.Socket
import Control.Exception (try, SomeException)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Concurrent.Async (mapConcurrently)
import Logger (LogLevel(..), logMessage)

-- Gerar lista de hosts para um segmento de rede
generateNetworkHosts :: String -> [String]
generateNetworkHosts baseIP = 
    let ipParts = take 3 $ splitOn '.' baseIP
        baseNetwork = concat $ zipWith (++) ipParts [".", ".", "."]
    in map (\x -> baseNetwork ++ show x) [1..254]

-- Dividir string por caractere
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = 
    let (prefix, suffix) = span (/= delim) str
    in prefix : case suffix of
        [] -> []
        (_:rest) -> splitOn delim rest

-- Verificar se um host está ativo usando ping
pingHost :: String -> IO Bool
pingHost host = do
    result <- try $ readProcessWithExitCode "ping" ["-c", "1", "-W", "1", host] ""
    case result of
        Left (_ :: SomeException) -> return False
        Right (exitCode, _, _) -> return $ exitCode == ExitSuccess

-- Descobrir hosts ativos na rede
discoverActiveHosts :: String -> IO [String]
discoverActiveHosts baseIP = do
    logMessage INFO $ "Iniciando descoberta de hosts ativos na rede " ++ baseIP ++ "/24"
    
    let networkHosts = take 20 $ generateNetworkHosts baseIP  -- Limitar a 20 hosts por performance
    
    logMessage DEBUG $ "Testando " ++ show (length networkHosts) ++ " endereços IP..."
    
    -- Ping todos os hosts concorrentemente
    results <- mapConcurrently pingHost networkHosts
    
    let activeHosts = map fst $ filter snd $ zip networkHosts results
    
    logMessage INFO $ "✓ Encontrados " ++ show (length activeHosts) ++ " hosts ativos"
    
    return activeHosts