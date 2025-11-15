module Logica
    ( ResultadoOperacao
    , addItem
    , removeItem
    , updateQty
    , criarLogFalha
    , buscarItem
    , calcularTotalInventario
    ) where

import InventarioTipos 
import qualified Data.Map as Map
import Data.Time (UTCTime)


criarLogFalha :: UTCTime -> AcaoLog -> String -> LogEntry
criarLogFalha tempo acao msg = LogEntry tempo acao msg (Falha msg)


buscarItem :: String -> Inventario -> Maybe Item
buscarItem idItem inventario = Map.lookup idItem inventario

calcularTotalInventario :: Inventario -> Int
calcularTotalInventario inventario = sum (map quantidade (Map.elems inventario))


addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem time novoItem inventario
    | quantidade novoItem < 0 =
        Left "Quantidade deve ser maior ou igual a zero"
    | Map.member (itemID novoItem) inventario =
        Left "Item com ID já existe no inventário"
    | otherwise =
        let inventarioNovo = Map.insert (itemID novoItem) novoItem inventario
            logEntry = LogEntry
                { timestamp = time
                , acao = Add
                , detalhes = "Adicionado item: " ++ nome novoItem ++ " (ID: " ++ itemID novoItem ++ ")"
                , status = Sucesso
                }
        in Right (inventarioNovo, logEntry)

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time itemID qtdRemover inventario
    | qtdRemover <= 0 =
        Left "Quantidade a remover deve ser positiva"
    | otherwise =
        case buscarItem itemID inventario of
            Nothing ->
                Left "Item não encontrado"
            Just item
                | quantidade item < qtdRemover ->
                    Left "Estoque insuficiente"
                | otherwise ->
                    let novaQuantidade = quantidade item - qtdRemover
                        itemAtualizado = item { quantidade = novaQuantidade }
                        inventarioNovo = if novaQuantidade == 0 
                                         then Map.delete itemID inventario
                                         else Map.insert itemID itemAtualizado inventario
                        logEntry = LogEntry
                            { timestamp = time
                            , acao = Remove
                            , detalhes = "Removidas " ++ show qtdRemover ++ " unidades do item " ++ itemID
                            , status = Sucesso
                            }
                    in Right (inventarioNovo, logEntry)

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time itemID novaQuantidade inventario
    | novaQuantidade < 0 =
        Left "Quantidade não pode ser negativa"
    | otherwise =
        case buscarItem itemID inventario of
            Nothing ->
                Left "Item não encontrado"
            Just item ->
                let itemAtualizado = item { quantidade = novaQuantidade }
                    inventarioNovo = Map.insert itemID itemAtualizado inventario
                    logEntry = LogEntry
                        { timestamp = time
                        , acao = Update
                        , detalhes = "Atualizada quantidade do item " ++ itemID ++ " para " ++ show novaQuantidade
                        , status = Sucesso
                        }
                in Right (inventarioNovo, logEntry)
