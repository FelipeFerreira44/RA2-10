module Analise
    ( logsDeErro
    , historicoPorItem
    , itemMaisMovimentado
    , formatarRelatorioCompleto
    , formatarLogEntry
    ) where

import InventarioTipos 
import Data.List (group, sort, sortBy, filter, isInfixOf, words)
import Data.Ord (comparing, Down(..))
import Data.Time (TimeZone, utcToZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErro
  where
    isErro logEntry = case status logEntry of
        Falha _ -> True
        Sucesso -> False

historicoPorItem :: [LogEntry] -> String -> [LogEntry]
historicoPorItem logs idItem =
    
    filter (\log -> ("item " ++ idItem) `isInfixOf` detalhes log) logs

itemMaisMovimentado :: [LogEntry] -> String
itemMaisMovimentado logs =
    let successLogs = filter (\log -> status log == Sucesso) logs
        extractIds :: String -> [String]
        extractIds det =
            let ws = words det
            in case dropWhile (/= "item") ws of
                 ("item":id:_) -> [id]
                 _ -> []
        allIds = concatMap (extractIds . detalhes) successLogs
        grouped = group (sort allIds)
        sorted = sortBy (comparing (Down . length)) grouped
    in case sorted of
        [] -> "Nenhum item movimentado"
        (ids:_) ->
            case ids of
                [] -> "Nenhum item"
                (itemId:_) -> "Item " ++ itemId ++ " (" ++ show (length ids) ++ " movimentações)"

formatarRelatorioCompleto :: TimeZone -> [LogEntry] -> String
formatarRelatorioCompleto tz logs =
    let totalLogs = length logs
        errorLogs = logsDeErro logs
        totalErrors = length errorLogs
        topItem = itemMaisMovimentado logs
        successRate = if totalLogs > 0
                     then (totalLogs - totalErrors) * 100 `div` totalLogs
                     else 0
    in unlines
        [ "=== RELATÓRIO DE ANÁLISE ==="
        , "Total de logs: " ++ show totalLogs
        , "Logs de erro: " ++ show totalErrors
        , "Taxa de sucesso: " ++ show successRate ++ "%"
        , "Item mais movimentado: " ++ topItem
        , ""
        , "Últimos logs de erro:"
        ] ++
        (if null errorLogs
         then "  Nenhum erro registrado\n"
         else unlines (map (formatarLogEntry tz) (take 3 (reverse errorLogs))))

formatarLogEntry :: TimeZone -> LogEntry -> String
formatarLogEntry tz logEntry =
    let timeStr = formatTime defaultTimeLocale "%H:%M:%S" (utcToZonedTime tz (timestamp logEntry))
        statusStr = case status logEntry of
            Sucesso -> "[SUCESSO]"
            Falha _ -> "[ERRO]"
    in "  " ++ timeStr ++ " " ++ statusStr ++ " " ++ detalhes logEntry
