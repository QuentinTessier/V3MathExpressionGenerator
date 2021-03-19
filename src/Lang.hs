{-# LANGUAGE BlockArguments #-}
module Lang where

import Control.Applicative
import Control.Monad
import Data.Char

import Parser.Combinator
import Syntax

symbols, lower, upper, digit, dot :: Parser Char
symbols = parseAnyOf "!#$%&*=+<.>/?@\\-^|~"
lower   = parseAnyOf ['a' .. 'z']
upper   = parseAnyOf ['A' .. 'Z']
digit   = parseAnyOf ['0' .. '9']
dot     = parseChar '.'

spacing, optSpacing :: Parser String
spacing = some (satisfy isSpace <|> (parseString "--" *> whileNot (parseChar '\n') *> parseChar '\n'))
optSpacing = many (satisfy isSpace <|> (parseString "--" *> whileNot (parseChar '\n') *> parseChar '\n'))

identifier :: Parser String
identifier = (:) <$> lower <*> many (upper <|> lower <|> digit)

nameTypeIdent :: Parser String
nameTypeIdent = (:) <$> upper <*> many (upper <|> lower <|> digit)

genericTypeIdent :: Parser String
genericTypeIdent = (:) <$> lower <*> many lower

typeIdent :: Parser Typename
typeIdent = identifier

integer :: Parser Integer
integer = do
          val <- some digit
          return (read val :: Integer)

float :: Parser Float
float = let p1 = do
                i <- some digit
                dot' <- dot
                d <- some digit
                return (i <> (dot' : d))
            p2 = do
                dot' <- dot
                d <- some digit
                return ('0' : dot' : d)
        in fmap (\s -> read s :: Float) (p1 <|> p2)

litInteger :: Parser Lit
litInteger = do LInt <$> integer

litFloat :: Parser Lit
litFloat = do LFloat <$> float

literal :: Parser Lit
literal = litFloat <|> litInteger ?? "Expected a Literal"

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = optSpacing >> parseString x >> optSpacing >> return f

op2 :: Parser (Expr -> Expr -> Expr)
op2 = infixOp "^" (Bin "Pow")

op3 :: Parser (Expr -> Expr -> Expr)
op3 = infixOp "*" (Bin "Mul") <|> infixOp "/" (Bin "Div") <|> infixOp "%" (Bin "Mod")

op4 :: Parser (Expr -> Expr -> Expr)
op4 = infixOp "+" (Bin "Add") <|> infixOp "-" (Bin "Sub")

op6 :: Parser (Expr -> Expr -> Expr)
op6 = infixOp "==" (Bin "Eqv") <|> infixOp "!=" (Bin "Dif")

op11 :: Parser (Expr -> Expr -> Expr)
op11 = infixOp "&&" (Bin "And")

op12 :: Parser (Expr -> Expr -> Expr)
op12 = infixOp "||" (Bin "And")

op14 :: Parser (Expr -> Expr -> Expr)
op14 = infixOp "=" (Bin "Assign")
        <|> infixOp "+=" (Bin "AssignAdd")
        <|> infixOp "-=" (Bin "AssignSub")
        <|> infixOp "*=" (Bin "AssignMul")
        <|> infixOp "/=" (Bin "AssignDiv")
        <|> infixOp "%=" (Bin "AssignMod")
        <|> infixOp "^=" (Bin "AssignPow")

assignOp :: Parser Expr
assignOp = orOp `chainl1` op14

orOp :: Parser Expr
orOp = andOp `chainl1` op12

andOp :: Parser Expr
andOp = compOp `chainl1` op11

compOp :: Parser Expr
compOp = addOp `chainl1` op6

addOp :: Parser Expr
addOp = mulOp `chainl1` op4

mulOp :: Parser Expr
mulOp = powOp `chainl1` op3

powOp :: Parser Expr
powOp = unary `chainl1` op2

assignment :: Parser Expr
assignment = do
            _ <- optSpacing
            var <- identifier
            fields <- fallback [] $ do
                _ <- parseChar '.'
                sepBy (parseChar '.') identifier
            _ <- optSpacing
            op <- parseString "=" <|> parseString "+=" <|> parseString "-=" <|> parseString "*=" <|> parseString "/=" <|> parseString "%=" <|> parseString "^="
            case op of
                "="  -> Bin "Assign" (Var var fields) <$> expr
                "+=" -> Bin "AssignAdd" (Var var fields) <$> expr
                "-=" -> Bin "AssignSub" (Var var fields) <$> expr
                "*=" -> Bin "AssignMul" (Var var fields) <$> expr
                "/=" -> Bin "AssignDiv" (Var var fields) <$> expr
                "%=" -> Bin "AssignMod" (Var var fields) <$> expr
                "^=" -> Bin "AssignPow" (Var var fields) <$> expr

unary :: Parser Expr
unary = do
        let p1 = do
                op <- parseAnyOf "-!"
                _ <- optSpacing
                rhs <- unary
                return case op of
                    '-' -> Un "Neg" rhs
                    '!' -> Un "Not" rhs
        p1 <|> postfix

postfix :: Parser Expr
postfix = let p1 = do Lit <$> literal
              p2 = do
                    name <- identifier
                    _ <- optSpacing >> parseChar '('
                    args <- sepBy (optSpacing *> parseChar ',') (optSpacing *> expr)
                    _ <- optSpacing >> parseChar ')'
                    return $ Call name args
              p3 = do
                   _ <- parseChar '(' >> optSpacing
                   exp <- expr
                   _ <- optSpacing >> parseChar ')'
                   return exp
              p4 = do
                    n <- identifier
                    fields <- fallback [] $ do
                        _ <- parseChar '.'
                        sepBy (parseChar '.') identifier
                    return $ Var n fields
              p5 = assignment
          in p5 <|> p2 <|> p4 <|> p3 <|> p1

plet :: Parser Expr
plet = do
        _ <- optSpacing *> parseString "let" *> optSpacing
        v <- variable
        _ <- optSpacing *> parseChar '=' *> optSpacing
        Let v <$> expr

expr :: Parser Expr
expr = plet <|> orOp

variable :: Parser Variable
variable = do
    name <- optSpacing *> identifier
    tname <- optSpacing *> parseChar ':' *> optSpacing *> typeIdent
    return $ Variable (name, tname)

formula :: Parser Stmt
formula = do
        _ <- optSpacing *> parseString "formula" *> optSpacing
        name <- identifier
        _ <- optSpacing *> parseChar '('
        args <- sepBy (optSpacing *> parseChar ',') (optSpacing *> variable)
        _ <- optSpacing *> parseChar ')' *> optSpacing *> parseString "=>" *> optSpacing
        ret <- typeIdent
        Formula name args <$> expr

tDefinition :: Parser Stmt
tDefinition = do
    _ <- optSpacing *> parseString "define" *> optSpacing *> parseString "tuple"
    _ <- optSpacing *> parseChar '<'
    fields <- sepBy (optSpacing *> parseChar ',') (optSpacing *> variable)
    _ <- optSpacing *> parseChar '>'
    name <- optSpacing *> parseString "as" *> optSpacing *> identifier
    return $ TypeDefinition name fields

gexpr :: Parser Stmt
gexpr = do
        _ <- optSpacing
        Expr <$> expr

stmt :: Parser Stmt
stmt = formula <|> gexpr <|> tDefinition