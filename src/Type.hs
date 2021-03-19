module Type where

import Utils.Miscs


aFloat, aInt, aVoid :: Type
aFloat = AtomicType "double"
aInt = AtomicType "int"
aVoid = AtomicType "void"

isAType :: Type -> (Name, Bool)
isAType (AtomicType name) = case name of
    "double" -> ("double", True)
    "int"    -> ("int", True)
    "void"   -> ("void", True)
    _        -> (name, False)

newtype Field = Field (Name, Type) deriving (Show, Eq)

data Type = AtomicType Name
          | UserType Name [Field]
          | FunctionType [Type] Type
          deriving (Show, Eq)

getFunctionRet :: Type -> Maybe Type
getFunctionRet (FunctionType _ r) = Just r
getFunctionRet _ = Nothing

getFunctionArgs :: Type -> Maybe [Type]
getFunctionArgs (FunctionType args _) = Just args
getFunctionArgs _ = Nothing

types :: [Type]
types = [
        AtomicType "int",
        AtomicType "double",
        AtomicType "void"
    ]

convertField :: Field -> (Name, Type)
convertField (Field t) = t

convertFields :: [Field] -> [(Name, Type)]
convertFields = map convertField

findField :: Name -> [Field] -> Maybe Type
findField n list = lookup n (convertFields list)

findVarType :: Type -> [Name] -> Maybe Type
findVarType t [] = Just t
findVarType (UserType _ fields) (x:xs) = case findField x fields of
    Just t -> findVarType t xs
    Nothing -> Nothing
findVarType t@(AtomicType _) (x:xs) = Nothing