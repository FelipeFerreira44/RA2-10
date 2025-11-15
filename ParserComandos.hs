module ParserComandos
    ( Comando(..)
    , parseComando
    , arquivoInventario
    , arquivoAuditoria
    ) where

import Text.Read (reads)


arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

arquivoAuditoria :: FilePath  
arquivoAuditoria = "Auditoria.log"


data Comando
    = CmdAdd String String Int String
    | CmdRemove String Int
    | CmdUpdate String Int
    | CmdList
    | CmdReport
    | CmdSearch String  
    | CmdHelp
    | CmdExit
    | CmdInvalido String
    deriving (Show, Eq)


parseComando :: String -> Comando
parseComando input =
    let tokens = words input
    in case tokens of
        ("add":idItem:nomeItem:qtd:categoria:_) ->
            case reads qtd of
                [(quantidade, "")] -> CmdAdd idItem nomeItem quantidade categoria
                _ -> CmdInvalido "Quantidade inválida para add"
        
        ("remove":idItem:qtd:_) ->
            case reads qtd of
                [(quantidade, "")] -> CmdRemove idItem quantidade
                _ -> CmdInvalido "Quantidade inválida para remove"
        
        ("update":idItem:qtd:_) ->
            case reads qtd of
                [(quantidade, "")] -> CmdUpdate idItem quantidade
                _ -> CmdInvalido "Quantidade inválida para update"

        ("search":termo:_) -> CmdSearch termo  
        
        ["list"] -> CmdList
        ["report"] -> CmdReport
        ["help"] -> CmdHelp
        ["exit"] -> CmdExit
        [] -> CmdInvalido "Comando vazio"
        _ -> CmdInvalido ("Comando desconhecido: " ++ unwords tokens)
