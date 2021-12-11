{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor
import Data.Char
import System.Console.Readline
import Control.Monad.Trans.State

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

-- try :: Parser s a -> Parser s a
-- try p = Parser $ \s ok err ->
--   case runParser p s of
--     Left _ -> err (ParseError []) s
--     Right a -> ok a s
--

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

optionnal :: (Alternative f, Monad f) => f (Maybe a) -> f (Maybe a)
optionnal p = p <|> return Nothing

consumeMaybe :: Parser s a -> Parser s (Maybe a)
consumeMaybe = fmap Just

liftMaybe :: Maybe a -> Parser s a
liftMaybe = maybe (parseError "Can't lift from Maybe") return

satisfy :: Show a => (a -> Bool) -> Parser [a] a
satisfy f = do
  c <- consumeOne
  if f c then return () else parseError $ "Unexpected character " ++ show c
  return c

char :: (Show a, Eq a) => a -> Parser [a] a
char = satisfy . (==)

digit :: Parser [Char] Integer
digit = (toInteger . digitToInt<$> satisfy isDigit) <?> "Awaited digit"

alphaNum :: Parser [Char] Char
alphaNum = satisfy isAlphaNum

naturalNumber :: Parser [Char] Integer
naturalNumber = do
  s <- some digit
  return $ foldl (\x y -> 10 * x + y) 0 s

nothing :: Parser [Char] a
nothing = return undefined

-- Parse sign, returns +1 if + or nothing is found, -1 else
sign :: Parser String Integer
sign = (char '-' >> return (-1)) <|> ((char '+' <|> nothing) >> return 1)

integralNumber :: Parser String Integer
integralNumber = do
  -- starts with - or (exclusive) starts with +
  s <- sign
  n <- naturalNumber

  return $ s * n

data SExpr
  = PlaceHolder
  | Paren SExpr
  | Identifier String
  | Biop String SExpr SExpr
  | UnaryOp String SExpr
  | Number Integer
  deriving (Show)

data SToken
  = TokNum Integer
  | TokDebug Char
  | TokIdentifier String
  | TokOp String
  | TokParenOpen
  | TokParenClose
  deriving (Show, Eq)

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

toTokens :: Parser [Char] [SToken]
toTokens = ((:[]) <$> debugToken <|> return []) <> (whitespaces *> many aToken <* eof)
  where
    debugToken = char ':' >> TokDebug <$> consumeOne
    aToken = (number <|> identifier <|> operator <|> paren) <* whitespaces
    number = TokNum <$> naturalNumber
    identifier = TokIdentifier <$> some alphaNum
    operator = TokOp <$> oneOf [token "+", token "-", token "*", token "/", token "^", token "="]
    paren = (char '(' >> return TokParenOpen) <|> (char ')' >> return TokParenClose)

hasGreaterPrecedence :: [Char] -> [Char] -> Bool
hasGreaterPrecedence a b = precedence a > precedence b
  where
    precedence :: String -> Integer
    precedence "=" = 0
    precedence "+" = 1
    precedence "-" = 1
    precedence "*" = 2
    precedence "/" = 2
    precedence "^" = 3
    precedence _ = undefined

eoe :: Parser [SToken] ()
eoe = eof <|> void (lookAhead (char TokParenClose)) <?> "An end of expression what expexted"

expression :: SExpr -> Parser [SToken] SExpr
expression PlaceHolder = consumeOne >>= f
  where
    f (TokNum n) = expression (Number n)
    f (TokIdentifier n) = expression (Identifier n)
    f (TokOp op) = expression (UnaryOp op PlaceHolder)
    f TokParenOpen = expression (Paren PlaceHolder)
    f t = parseError $ "Token " ++ show t ++ " can't be at start of an expression"
expression (Paren PlaceHolder) = do
  s <- Paren <$> expression PlaceHolder
  void (char TokParenClose) <|> parseError "Closing parenthesis not found"
  expression s
expression (UnaryOp op PlaceHolder) = (consumeOne >>= f) <?> ("Unary operation " ++ op ++ " is missing his right hand side")
  where
    f (TokNum n) = expression (UnaryOp op (Number n))
    f (TokIdentifier n) = expression (UnaryOp op (Identifier n))
    f TokParenOpen = UnaryOp op <$> expression (Paren PlaceHolder)
    f _ = empty
expression (Biop "=" t@(Identifier _) PlaceHolder) = (Biop "=" t <$> expression PlaceHolder) <?> "Assignement is missing his right hand side"
expression (Biop "=" _ PlaceHolder) =  parseError "Assignement should have an identifier as rhs"
expression (Biop op a PlaceHolder) = (consumeOne >>= f) <?> ("Binary operation " ++ op ++ " is missing his right hand side")
  where
    f (TokNum n) = expression (Biop op a (Number n))
    f (TokIdentifier n) = expression (Biop op a (Identifier n))
    f TokParenOpen = Biop op a <$> expression (Paren PlaceHolder)
    f _ = empty
expression bop@(Biop op a b) = (consumeOne >>= f) <|> (eoe >> return bop)
  where
    f (TokOp op') =
      if hasGreaterPrecedence op' op
        then Biop op a <$> expression (Biop op' b PlaceHolder)
        else expression (Biop op' bop PlaceHolder)
    f _ = empty
expression n = (eoe >> return n) <|> (consumeOne >>= f)
  where
    f (TokOp op) = expression (Biop op n PlaceHolder)
    f _ = empty

-- This small language parses math expressions containing integers, + ^ * and parentheses
toAst :: Parser [SToken] SExpr
toAst = expression PlaceHolder <* eof

newtype EvalError = EvalError {runEvalError :: [String]} deriving (Show)

type EvalT a = StateT (Map String Integer) (Either EvalError) a

evalError :: String -> EvalT a
evalError = lift . Left . EvalError . (:[])

eval :: SExpr -> EvalT Integer
eval (Number n) = lift $ Right n
eval (Identifier s) = get >>= maybe (evalError $ "Identifier " ++ s ++ " not found") return . Map.lookup s
eval (Paren (eval -> n)) = n
eval (UnaryOp "-" n) = negate <$> eval n
eval (UnaryOp op _) = evalError $ "Unknown unary operator " ++ op
eval (Biop "=" (Identifier lhs) (eval -> rhs)) = rhs >>= (\x -> modify (Map.insert lhs x) >> return x)
eval (Biop "=" lhs _) = evalError $ "Lhs of assignement have to be an identifier, found " ++ show lhs
eval (Biop "+" (eval -> lhs) (eval -> rhs)) = liftM2 (+) lhs rhs
eval (Biop "-" (eval -> lhs) (eval -> rhs)) = liftM2 (-) lhs rhs
eval (Biop "*" (eval -> lhs) (eval -> rhs)) = liftM2 (*) lhs rhs
eval (Biop "/" (eval -> lhs) (eval -> rhs)) = do
    a <- lhs
    b <- rhs
    lift $ a `safe_div` b
  where
    safe_div _ 0 = Left . EvalError $ ["Division by 0 !"]
    safe_div a b = Right $ a `div` b
eval (Biop "^" (eval -> lhs) (eval -> rhs)) = liftM2 (^) lhs rhs
eval (Biop op _ _) = lift .Left . EvalError $ ["Unknown binary operator " ++ op]
eval PlaceHolder = return 0

printError :: String -> Either [String] b -> MaybeT IO b
printError s (Left msgs) = lift (putStrLn ("Error during " ++ s ++ ":") >> foldMap (putStrLn . color Yellow) msgs) >> hoistMaybe Nothing
printError _ (Right a) = return a

printTokenError :: Either ParseError [SToken] -> MaybeT IO [SToken]
printTokenError = printError "tokenizer" . first runParseError

printAstError :: Either ParseError SExpr -> MaybeT IO SExpr
printAstError = printError "AST generation" . first runParseError

printEvalError :: Either EvalError a -> MaybeT IO a
printEvalError = printError "Evaluation" . first runEvalError

runAndEval :: Map String Integer -> String -> IO (Map String Integer)
runAndEval m s = runMaybeT (do
  t <- printTokenError $ runParser toTokens s

  (tokens, debugKind) <- return $ getDebugInfo t
  when (debugKind == 'h') (lift printHelp >> hoistMaybe Nothing)
  when (debugKind == 'm') (lift (print m) >> hoistMaybe Nothing)
  when (debugKind == 't') (lift (print tokens) >> hoistMaybe Nothing)

  ast <- printAstError $ runParser toAst tokens
  when (debugKind == 'a') (lift (print ast) >> hoistMaybe Nothing)

  (e', m') <- printEvalError $ runStateT (eval ast) m
  lift $ print e'
  return m'
  ) >>= maybe (return m) return

printHelp :: IO ()
printHelp =putStr $ unlines [
          "This program can evaluate some simple expression",
          "Only integer number are supported",
          "The supported operations are +,-,/,^",
          "You can also assignate values to variables, using the syntax `variable = value` ",
          "Debug features:",
          "You can show some internal structures by providing a debug flag",
          "such debug flag is ':c' where c is a character which will dictate the kind of debug",
          ":a to see ast, :t to see the tokens, :h to print this help, :m to see the stored variables values"
          ]

getDebugInfo :: [SToken] -> ([SToken], Char)
getDebugInfo (TokDebug d:xs) = (xs, d)
getDebugInfo t = (t, 'k')

-- Interaction with "outside"

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Reset

colorToEscapeCode :: Color -> String
colorToEscapeCode c = escKey ++ colorCode c ++ "m"
  where
    escKey = "\27["
    colorCode Black = "30"
    colorCode Red = "31"
    colorCode Green = "32"
    colorCode Yellow = "33"
    colorCode Blue = "34"
    colorCode Magenta = "35"
    colorCode Cyan = "36"
    colorCode White = "37"
    colorCode Reset = "0"

color :: Color -> String -> String
color (colorToEscapeCode -> e) s = e ++ s ++ colorToEscapeCode Reset

repl :: IO ()
repl = repl' (Map.empty :: Map String Integer)
  where
    repl' m = do
      maybeLine <- readline ">>> "
      case maybeLine of
        Nothing -> return () -- EOF / control-d
        Just line -> do
          addHistory line
          m' <- runAndEval m line
          repl' m'

main :: IO ()
main = repl
