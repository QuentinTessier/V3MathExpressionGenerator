module Utils.Miscs where

import qualified Data.Map as Map

type Name = String

newtype SymbolTable a b = SymTable (Map.Map a b) deriving (Show, Eq)