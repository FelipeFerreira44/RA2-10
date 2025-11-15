{-# LANGUAGE DeriveGeneric #-}
module InventarioTipos 
    ( Item(..)
    , Inventario
    , AcaoLog(..)
    , StatusLog(..)
    , LogEntry(..)
    , ResultadoOperacao
    , emptyInventario
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)


data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog
    = Add
    | Remove
    | Update
    | List
    | Report
    | QueryFail
    deriving (Show, Read, Eq)

data StatusLog
    = Sucesso
    | Falha String
    deriving (Show, Read, Eq)

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read, Eq)

type ResultadoOperacao = (Inventario, LogEntry)

emptyInventario :: Inventario
emptyInventario = Map.empty
