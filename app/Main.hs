module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import Scanner (scanPortWithTimeout)
import OSFingerprint (fingerprintOS)
import Logger (LogLevel(..), logMessage)

-- Função principal
main :: IO ()
main = do
    logMessage INFO "Network Scanner iniciando..."
    
    let host = "127.0.0.1"
    let timeoutSeconds = 3
    
    -- Porta específicas e intervalos de portas expandidos
    let commonPorts = [21, 22, 23, 25, 53, 80, 110, 143, 443, 993, 995, 3306, 3389, 5432, 5900]
    let webPorts = [8080, 8443, 9000]
    let customRange = [75..85]
    let allPorts = commonPorts ++ webPorts ++ customRange
    
    logMessage INFO $ "Escaneando " ++ show (length allPorts) ++ " portas no host " ++ host
    logMessage DEBUG $ "Portas a serem escaneadas: " ++ show allPorts
    
    -- Escaneia de forma concorrente com timeout
    results <- mapConcurrently (scanPortWithTimeout timeoutSeconds host) allPorts
    
    -- Obter data e hora atuais para relatório
    time <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" time
    let reportHeader = "=== NETWORK SCAN REPORT ===\n" ++
                      "Generated at: " ++ timestamp ++ "\n" ++
                      "Target: " ++ host ++ "\n" ++
                      "Timeout: " ++ show timeoutSeconds ++ " seconds\n" ++
                      "Total ports scanned: " ++ show (length allPorts) ++ "\n" ++
                      "===============================\n"
    
    -- Realiza fingerprinting do SO
    logMessage INFO "Realizando fingerprinting do sistema operacional..."
    osFingerprint <- fingerprintOS host
    
    -- Separa portas abertas e fechadas
    let openPorts = filter (elem '✓') results
    let closedPorts = filter (elem '✗') results
    
    -- Imprime os resultados no console
    putStrLn "\n=== SCAN RESULTS ==="
    putStrLn $ "Portas abertas encontradas: " ++ show (length openPorts)
    mapM_ putStrLn openPorts
    
    putStrLn $ "\nPortas fechadas/filtradas: " ++ show (length closedPorts)
    logMessage DEBUG "Mostrando primeiras 10 portas fechadas:"
    mapM_ putStrLn (take 10 closedPorts)
    
    putStrLn $ "\n" ++ osFingerprint
    
    -- Gera relatório detalhado em arquivo
    let fullReport = reportHeader ++ 
                    "\nPORTAS ABERTAS (" ++ show (length openPorts) ++ "):\n" ++
                    unlines openPorts ++
                    "\nPORTAS FECHADAS/FILTRADAS (" ++ show (length closedPorts) ++ "):\n" ++
                    unlines closedPorts ++
                    "\nSISTEMA OPERACIONAL:\n" ++
                    osFingerprint ++ "\n" ++
                    "\n=== END OF REPORT ==="
    
    writeFile "detailed_scan_report.txt" fullReport
    logMessage INFO "Relatório detalhado salvo em 'detailed_scan_report.txt'"
    logMessage INFO "Scan concluído com sucesso!"