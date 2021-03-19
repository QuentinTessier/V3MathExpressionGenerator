{-# LANGUAGE LambdaCase #-}

module Parser.Combinator where

import Control.Applicative ( Alternative(..) )
import Control.Monad ( MonadPlus(..) )

data ParseFailure = MatchFailed String
                  | EOI
    deriving (Eq)

instance Show ParseFailure where
    show (MatchFailed s) = s
    show EOI           = "Found a unexpected end of input"

data ParseOutcome a = Success a
                    | Failure ParsePosition ParseFailure
    deriving (Eq)

instance Show a => Show (ParseOutcome a) where
    show (Success a) = show a
    show (Failure pos err) = "Failed to parse " <> show err <> " at " <> show pos

instance Functor ParseOutcome where
    fmap f p = case p of
                (Failure pos err) -> Failure pos err
                (Success a)       -> Success (f a)

instance Applicative ParseOutcome where
    pure = Success
    Failure pos err <*> _ = Failure pos err
    Success f       <*> x = fmap f x

instance Alternative ParseOutcome where
    empty = Failure emptyPosition $ MatchFailed "Unknown error"
    Success a    <|> _            = Success a
    Failure _ _  <|> Success b    = Success b
    Failure p1 a <|> Failure p2 b = if p1 > p2
                                        then Failure p1 a
                                        else Failure p2 b

instance Monad ParseOutcome where
    return = pure
    Failure pos err >>= _ = Failure pos err
    Success x       >>= f = f x

data ParsePosition = ParsePosition {
    line :: !Integer,
    column :: !Integer,
    file :: Maybe FilePath
} deriving (Eq)

instance Show ParsePosition where
    show (ParsePosition line column file) = case file of
                                                Just f  -> f <> "(" <> show line <> ":" <> show column <> ")"
                                                Nothing -> show line <> ":" <> show column

instance Ord ParsePosition where
    compare (ParsePosition l1 c1 _) (ParsePosition l2 c2 _) = compare l1 l2 <> compare c1 c2

emptyPosition :: ParsePosition
emptyPosition = ParsePosition 0 0 Nothing

newtype Parser a = Parser ((String, ParsePosition) -> ParseOutcome (a, (String, ParsePosition)))

parse :: Parser a -> String -> ParseOutcome (a, (String, ParsePosition))
parse (Parser f) input = f (input, ParsePosition 1 1 Nothing)

parseIO :: Parser a -> String -> IO (ParseOutcome (a, (String, ParsePosition)))
parseIO (Parser f) input = do
                            contents <- readFile input
                            return $ f (contents, ParsePosition 1 1 $ Just input)

parseAt :: Parser a -> (String, ParsePosition) -> ParseOutcome (a, (String, ParsePosition))
parseAt (Parser f) = f

instance Functor Parser where
    fmap f (Parser p) = Parser $ \str -> do
                                        (a, str') <- p str
                                        return (f a, str')

instance Applicative Parser where
    pure a = Parser $ \str -> Success (a, str)
    Parser p1 <*> p2 = Parser $ \str -> do
                                            (f, str') <- p1 str
                                            parseAt (fmap f p2) str'

instance MonadPlus Parser where
    mzero = Parser $ \(_, pos) -> Failure pos $ MatchFailed "Unknown error"
    mplus (Parser a) (Parser b) = Parser $ \str -> a str <|> b str

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance Monad Parser where
    return = pure
    Parser a >>= f = Parser $ \str -> do
                                (ret, str') <- a str
                                parseAt (f ret) str'

(??) :: Parser a -> String -> Parser a
Parser f ?? err = Parser $ \str ->
                    case f str of
                        Success ret   -> return ret
                        Failure pos _ -> Failure pos (MatchFailed err)
infixl 0 ??

updatePosition :: Char -> ParsePosition -> ParsePosition
updatePosition '\n' (ParsePosition l c f) = ParsePosition (l + 1) 1 f
updatePosition _ (ParsePosition l c f) = ParsePosition l (c + 1) f

parserPosition :: Parser ParsePosition
parserPosition = Parser $ \(str, pos) -> Success (pos, (str, pos))

peek :: Parser a -> Parser a
peek p = Parser $ \str -> do
                    (a, _) <- parseAt p str
                    return (a, str)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
                    ([], pos) -> Failure pos EOI
                    (c:cs, pos) -> if f c
                        then Success (c, (cs, updatePosition c pos))
                        else Failure pos $ MatchFailed $ "Unexpected token: " <> show c

choice :: [Parser a] -> Parser a
choice = foldl (<|>) empty

fallback :: a -> Parser a -> Parser a
fallback a p = p <|> pure a

enclosed :: Parser a -> Parser b -> Parser c -> Parser c
enclosed open close item = open *> item <* close

sepBy1 :: Parser b -> Parser a -> Parser [a]
sepBy1 sep item = (:) <$> item <*> many (sep *> item)

sepBy :: Parser b -> Parser a -> Parser [a]
sepBy sep = fallback [] . sepBy1 sep

parseEnd :: Parser ()
parseEnd = Parser $ \case
                    ([], pos) -> Success ((), ("", pos))
                    (c:_, pos) -> Failure pos $ MatchFailed $ "Expected a end of input, got" <> show c

parseAny :: Parser Char
parseAny = Parser $ \case
                    ([], pos) -> Failure pos EOI
                    (c:cs, pos) -> Success (c, (cs, updatePosition c pos))

parseNothing :: Parser ()
parseNothing = pure ()

parseChar :: Char -> Parser Char
parseChar c = Parser $ \case
                        ([], pos) -> Failure pos EOI
                        (x:xs, pos) | x == c -> Success (c, (xs, updatePosition x pos))
                        (x:_, pos) -> Failure pos $ MatchFailed $ "Expected: " <> show c <> ", got: " <> show x

parseNotChar :: Char -> Parser Char
parseNotChar c = Parser $ \case
                           ([], pos) -> Failure pos EOI
                           (x:xs, pos) | x /= c -> Success (c, (xs, updatePosition x pos))
                           (x:_, pos) -> Failure pos $ MatchFailed $ "Expected anything except: " <> show c <> ", got" <> show x

parseAnyOf :: String -> Parser Char
parseAnyOf = choice . map parseChar

parseString :: String -> Parser String
parseString = mapM parseChar

parseTry :: Parser a -> Parser ()
parseTry p = Parser $ \str ->
                case parseAt (peek p) str of
                    Success (_, (_, pos))     -> Failure pos $ MatchFailed "Invalid input"
                    Failure _ (MatchFailed _) -> Success ((), str)
                    Failure pos EOI           -> Failure pos EOI

sequenceOf :: (Char -> Bool) -> Parser String
sequenceOf f = some $ satisfy f


space :: Parser Char
space = parseAnyOf "\r\n\t "

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
    where rest a = (do f <- op
                       b <- p
                       rest (f a b))
                    <|> return a

whileNot :: Parser a -> Parser String
whileNot p = Parser $ \s ->
    case parseAt (peek p) s of
        Success (_, rest) -> Success ([], rest)
        Failure _ (MatchFailed _) ->
            let recur = ((:) <$> parseAny <*> whileNot p)
            in parseAt recur s
        Failure pos EOI -> Failure pos EOI
