{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolymorphicComponents #-}

module Parser
  ( Parser,
    ParseError(runParseError),
    runParser,
    runParserGetUnparsed,
    notFollowedBy,
    parseError,
    consumeOne,
    token,
    lookAhead,
    eof,
    (<?>),
    satisfy,
    char,
    oneOf,
    till,
    whitespace,
    whitespaces,
  )
where

import Control.Applicative
import Control.Monad

newtype ParseError = ParseError {runParseError :: [String]}
  deriving (Show)

instance Semigroup ParseError where
  a <> b = ParseError $ runParseError a <> runParseError b

newtype Parser s a = Parser
  { unParser ::
      forall b.
      s -> -- To be consumed
      (a -> s -> b) -> -- ok, consume
      (ParseError -> s -> b) -> -- error, fail
      b -- output
  }

runParser :: Parser s a -> s -> Either ParseError a
runParser p s = unParser p s (const . Right) (const . Left)

runParserGetUnparsed :: Parser s a -> s -> Either (ParseError, s) (a, s)
runParserGetUnparsed p s = unParser p s (curry Right) (curry Left)

-- Does not consume or perform side effects !
-- Could return a Boolean without loss of information
notFollowedBy :: Parser s a -> Parser s ()
notFollowedBy p = Parser $ \s ok err ->
  case runParser p s of
    Right _ -> err (ParseError ["Expected failure"]) s
    Left _ -> ok () s

parseError :: [Char] -> Parser s a
parseError msg = Parser $ \s _ err -> err (ParseError [msg]) s

instance Semigroup a => Semigroup (Parser s a) where
  p <> q = do
    x <- p
    y <- q
    return $ x <> y

instance Monad (Parser s) where
  return a = Parser $ \s ok _ -> ok a s
  p >>= f = parserBind p f

parserBind :: Parser s a -> (a -> Parser s b) -> Parser s b
parserBind p f = Parser $ \s ok err ->
  let new_ok a s2 = unParser (f a) s2 ok err
   in unParser p s new_ok err

instance Applicative (Parser s) where
  pure = return
  (<*>) = Control.Monad.ap

instance Functor (Parser s) where
  fmap f p = parserMap f p

instance Alternative (Parser s) where
  empty = parseError "Unknown error"
  (<|>) = mplus

instance MonadPlus (Parser s) where
  mzero = empty
  mplus m n = parserPlus m n

parserPlus :: Parser s a -> Parser s a -> Parser s a
parserPlus m n = Parser $ \s ok err ->
  let new_err _ _ = unParser n s ok err
   in unParser m s ok new_err

parserMap :: (a -> b) -> Parser s a -> Parser s b
parserMap f p = Parser $ \s ok err ->
  unParser p s (ok . f) err

consumeOne :: Parser [a] a
consumeOne = Parser $ \s ok err ->
  let walk (c : xs) = ok c xs
      walk s2 = err (ParseError ["End of file not expected"]) s2
   in walk s

-- Consume a string from input
token :: String -> Parser String String
token tok = Parser $ \s ok err ->
  let walk ss [] = ok tok ss
      walk (r : rs) (t : ts)
        | r == t = walk rs ts
        | otherwise = err (ParseError ["Expected token " ++ tok]) s
      walk _ _ = err (ParseError [tok]) s
   in walk s tok

lookAhead :: Parser a b -> Parser a b
lookAhead p =
  Parser
    ( \s ok err ->
        let new_err e _ = err e s
            new_ok a _ = ok a s
         in unParser p s new_ok new_err
    )

eof :: Show a => Parser [a] ()
eof = eof' <|> (till eof' >>= \x -> parseError ("Expected end of file, found " ++ show x))
  where
    eof' = notFollowedBy consumeOne

(<?>) :: Parser s a -> [Char] -> Parser s a
p <?> msg = p <|> parseError msg

satisfy :: Show a => (a -> Bool) -> Parser [a] a
satisfy f = do
  c <- consumeOne
  if f c then return () else parseError $ "Unexpected character " ++ show c
  return c

char :: (Show a, Eq a) => a -> Parser [a] a
char = satisfy . (==)

oneOf :: [Parser String String] -> Parser String String
oneOf = foldl (<|>) mzero

till :: Parser [a] b -> Parser [a] [a]
till p =
  (lookAhead p >> return [])
    <|> do
      x <- consumeOne
      xs <- till p
      return $ x : xs

whitespace :: Parser [Char] ()
whitespace = void $ char ' ' <|> char '\t' <|> char '\n'

whitespaces :: Parser [Char] ()
whitespaces = void (many whitespace)
