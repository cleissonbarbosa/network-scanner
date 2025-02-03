module Main (main) where

import Network.Socket
import Control.Exception (try, IOException)
import Control.Concurrent.Async (mapConcurrently)
import System.IO (writeFile)
import Data.Time (getCurrentTime)
import qualified Data.ByteString.Char8 as B
import Scanner (scanPort)
import OSFingerprint (fingerprintOS)

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