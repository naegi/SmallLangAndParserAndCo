{-# LANGUAGE ViewPatterns #-}

module Lang
  ( SToken (TokDebug),
    SExpr,
    EvalError (runEvalError),
    toTokens,
    toAst,
    eval,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Parser

digit :: Parser [Char] Integer
digit = (toInteger . digitToInt <$> satisfy isDigit) <?> "Awaited digit"

alphaNum :: Parser [Char] Char
alphaNum = satisfy isAlphaNum

naturalNumber :: Parser [Char] Integer
naturalNumber = do
  s <- some digit
  return $ foldl (\x y -> 10 * x + y) 0 s

-- Parse sign, returns +1 if + or nothing is found, -1 else
sign :: Parser String Integer
sign = (char '-' >> return (-1)) <|> ((char '+' <|> mzero) >> return 1)

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

toTokens :: Parser [Char] [SToken]
toTokens = ((: []) <$> debugToken <|> return []) <> (whitespaces *> many aToken <* eof)
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
expression (Biop "=" _ PlaceHolder) = parseError "Assignement should have an identifier as rhs"
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
evalError = lift . Left . EvalError . (: [])

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
eval (Biop op _ _) = lift . Left . EvalError $ ["Unknown binary operator " ++ op]
eval PlaceHolder = return 0
