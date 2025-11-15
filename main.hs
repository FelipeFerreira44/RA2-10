module Main where

import InventarioTipos
import Logica
import ParserComandos
import Analise

import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.LocalTime (TimeZone(..))
import System.IO (hFlush, stdout)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)
import Data.List (isInfixOf)

salvarInventario :: Inventario -> IO ()
salvarInventario inventario = do
    writeFile arquivoInventario (show inventario)
    putStrLn "Inventário salvo em Inventario.dat"

adicionarLogAuditoria :: LogEntry -> IO ()
adicionarLogAuditoria logEntry = do
    appendFile arquivoAuditoria (show logEntry ++ "\n")
    putStrLn "Log registrado em Auditoria.log"

carregarInventario :: IO Inventario
carregarInventario = catch (do
    conteudo <- readFile arquivoInventario
    case readMaybe conteudo of
        Just inv -> return inv
        Nothing -> return emptyInventario
    ) handler
  where
    handler e
        | isDoesNotExistError e = return emptyInventario
        | otherwise = ioError e

carregarLogs :: IO [LogEntry]
carregarLogs = catch (do
    conteudo <- readFile arquivoAuditoria
    let linhas = lines conteudo
        logs = mapMaybe readMaybe linhas
    return logs
    ) handler
  where
    handler e
        | isDoesNotExistError e = return []
        | otherwise = ioError e

imprimirItem :: Item -> IO ()
imprimirItem item = do
    putStrLn $ "  " ++ itemID item ++ " | " ++ nome item ++
                 " | Qtd: " ++ show (quantidade item) ++ " | Cat: " ++ categoria item

mostrarAjuda :: IO ()
mostrarAjuda = do
    putStrLn "\n=== COMANDOS DISPONÍVEIS ==="
    putStrLn "add ID nome quantidade categoria"
    putStrLn "remove ID quantidade"
    putStrLn "update ID nova_quantidade"
    putStrLn "search TERMO      - Buscar itens"
    putStrLn "list              - Listar inventário"
    putStrLn "report            - Relatório de logs"
    putStrLn "help              - Esta mensagem"
    putStrLn "exit              - Encerrar"
    putStrLn "============================"

buscarItens :: String -> Inventario -> IO ()
buscarItens termo inventario = do
    let resultados = Map.filter (\item ->
             termo `isInfixOf` itemID item ||
             termo `isInfixOf` nome item ||
             termo `isInfixOf` categoria item) inventario

    if Map.null resultados
        then putStrLn "Nenhum item encontrado."
        else do
            putStrLn $ "\nResultados para '" ++ termo ++ "':"
            mapM_ imprimirItem (Map.elems resultados)

processarComando :: Comando -> Inventario -> [LogEntry] -> IO (Inventario, [LogEntry])
processarComando cmd inventario logs = do
    tempo <- getCurrentTime

    case cmd of
        CmdAdd idItem nomeItem qtd cat -> do
            let novoItem = Item idItem nomeItem qtd cat
            case addItem tempo novoItem inventario of
                Left erro -> do
                    let logEntry = criarLogFalha tempo Add erro
                    adicionarLogAuditoria logEntry
                    putStrLn $ "Erro: " ++ erro
                    return (inventario, logEntry:logs)
                Right (novoInventario, logEntry) -> do
                    salvarInventario novoInventario
                    adicionarLogAuditoria logEntry
                    putStrLn "Item adicionado com sucesso!"
                    return (novoInventario, logEntry:logs)

        CmdRemove idItem qtd -> do
            case removeItem tempo idItem qtd inventario of
                Left erro -> do
                    let logEntry = criarLogFalha tempo Remove erro
                    adicionarLogAuditoria logEntry
                    putStrLn $ "Erro: " ++ erro
                    return (inventario, logEntry:logs)
                Right (novoInventario, logEntry) -> do
                    salvarInventario novoInventario
                    adicionarLogAuditoria logEntry
                    putStrLn "Item removido com sucesso!"
                    return (novoInventario, logEntry:logs)

        CmdUpdate idItem novaQtd -> do
            case updateQty tempo idItem novaQtd inventario of
                Left erro -> do
                    let logEntry = criarLogFalha tempo Update erro
                    adicionarLogAuditoria logEntry
                    putStrLn $ "Erro: " ++ erro
                    return (inventario, logEntry:logs)
                Right (novoInventario, logEntry) -> do
                    salvarInventario novoInventario
                    adicionarLogAuditoria logEntry
                    putStrLn "Quantidade atualizada com sucesso!"
                    return (novoInventario, logEntry:logs)

        CmdSearch termo -> do
            buscarItens termo inventario
            return (inventario, logs)

        CmdList -> do
            putStrLn "\n=== INVENTÁRIO ATUAL ==="
            if Map.null inventario
                then putStrLn "Inventário vazio."
                else mapM_ imprimirItem (Map.elems inventario)
            putStrLn $ "Total de itens: " ++ show (Map.size inventario)
            putStrLn $ "Total em estoque: " ++ show (calcularTotalInventario inventario) ++ " unidades"
            return (inventario, logs)

        CmdReport -> do
            let tz = TimeZone (-180) False "BRT"


            putStrLn $ "\n" ++ formatarRelatorioCompleto tz logs


            putStrLn "\n--- HISTÓRICO DETALHADO ---"
            putStr "Digite o ID do item para ver o histórico (ou ENTER para pular): "
            hFlush stdout
            idInput <- getLine

            if null idInput
            then return (inventario, logs)
            else do
                let historico = historicoPorItem logs idInput
                putStrLn $ "Histórico de logs para ID '" ++ idInput ++ "':"

                if null historico
                then putStrLn "  Nenhum registro encontrado."
                else mapM_ (putStrLn . formatarLogEntry tz) historico

                return (inventario, logs)

        CmdHelp -> do
            mostrarAjuda
            return (inventario, logs)

        CmdExit -> do
            putStrLn "Encerrando sistema..."
            return (inventario, logs)

        CmdInvalido msg -> do
            putStrLn msg
            return (inventario, logs)

loopPrincipal :: Inventario -> [LogEntry] -> IO ()
loopPrincipal inventario logs = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let cmd = parseComando input

    if cmd == CmdExit
        then return ()
        else do
            (novoInventario, novosLogs) <- processarComando cmd inventario logs
            loopPrincipal novoInventario novosLogs

popularInventarioInicial :: Inventario -> IO Inventario
popularInventarioInicial inventario = do
    tempo <- getCurrentTime
    let itensIniciais = [
            Item "001" "Teclado Mecânico" 50 "Periféricos",
            Item "002" "Mouse Óptico" 75 "Periféricos",
            Item "003" "Monitor 24\"" 30 "Monitores",
            Item "004" "Cadeira Gamer" 15 "Móveis",
            Item "005" "Headphone Bluetooth" 40 "Áudio",
            Item "006" "Webcam HD" 25 "Vídeo",
            Item "007" "SSD 1TB" 60 "Armazenamento",
            Item "008" "Memória RAM 16GB" 45 "Memória",
            Item "009" "Processador i7" 20 "CPU",
            Item "010" "Placa de Vídeo RTX" 10 "Gráficos"
            ]

    let processarItem :: Inventario -> Item -> IO Inventario
        processarItem inv item =
            case addItem tempo item inv of
                Left _ -> return inv
                Right (novoInv, logEntry) -> do
                    adicionarLogAuditoria logEntry
                    return novoInv

    inventarioFinal <- foldM processarItem inventario itensIniciais
    salvarInventario inventarioFinal  
    return inventarioFinal

main :: IO ()
main = do
    putStrLn "=== SISTEMA DE GERENCIAMENTO DE INVENTÁRIO ==="

    inventario <- carregarInventario
    logs <- carregarLogs

    putStrLn $ "Itens carregados: " ++ show (Map.size inventario)
    putStrLn $ "Logs carregados: " ++ show (length logs)

    inventarioFinal <- if Map.null inventario
        then do
            putStrLn "Populando com dados iniciais (10 itens)..."
            popularInventarioInicial inventario
        else return inventario

    mostrarAjuda
    loopPrincipal inventarioFinal logs
