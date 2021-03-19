{-# LANGUAGE LambdaCase #-}

module Syntax where

import Utils.Miscs
import Data.List (intercalate)

type Typename = String

data Assoc a = AssocLeft a
             | AssocRight a
             deriving (Show, Eq, Ord)

precedence :: Assoc a -> a
precedence = \case
        (AssocRight v) -> v
        (AssocLeft v) -> v

data Lit = LInt Integer
         | LFloat Float
         deriving (Show, Eq, Ord)

newtype Variable = Variable (Name, Typename) deriving (Show, Eq)

data Expr = Var Name [Name]
          | Lit Lit
          | Bin Name Expr Expr
          | Un Name Expr
          | Call Name [Expr]
          | Let Variable Expr
          deriving (Eq)

data Stmt = Formula Name [Variable] Expr
          | Expr Expr
          | TypeDefinition Name [Variable]
          deriving (Eq)

instance Show Expr where
    show (Lit l) = case l of
                    (LInt i) -> show i
                    (LFloat f) -> show f
    show (Bin op lhs rhs) = op ++ " " ++ show lhs ++ " " ++ show rhs
    show (Un op rhs) = op ++ " " ++ show rhs
    show (Call name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (Let (Variable (name, t)) val) = name ++ " : " ++ show t ++ " = " ++ show val
    show (Var n fields) = n ++ "." ++ intercalate "." (map show fields)

instance Show Stmt where
    show (Formula n types body) = n ++ intercalate " -> " (map show types) ++ show body
    show (TypeDefinition n fields) = n ++ " {\n\t" ++ intercalate ",\n\t" (map show fields) ++ "\n}\n"
    show (Expr exp) = show exp

baseBinaryOperator :: [(String, (Name, Assoc Integer))]
baseBinaryOperator = [
        ("+", ("Add", AssocLeft 4)),
        ("-", ("Sub", AssocLeft 4)),
        ("*", ("Mul", AssocLeft 3)),
        ("/", ("Div", AssocLeft 3)),
        ("%", ("Mod", AssocLeft 3)),
        ("^", ("Pow", AssocLeft 2)),
        ("==", ("Eqv", AssocLeft 6)),
        ("!=", ("Dif", AssocLeft 6)),
        ("&&", ("And", AssocLeft 11)),
        ("||", ("Or", AssocLeft 12)),
        ("=", ("Assign", AssocRight 14)),
        ("+=", ("AssignAdd", AssocRight 14)),
        ("-=", ("AssignSub", AssocRight 14)),
        ("*=", ("AssignMul", AssocRight 14)),
        ("/=", ("AssignDiv", AssocRight 14)),
        ("%=", ("AssignMod", AssocRight 14)),
        ("^=", ("AssignPow", AssocRight 14))
    ]

findOperator :: String -> Maybe (Name, Assoc Integer)
findOperator symbol = lookup symbol baseBinaryOperator