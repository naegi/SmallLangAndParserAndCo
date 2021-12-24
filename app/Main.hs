{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT (runStateT))
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Base (when)
import Lang
import Parser (ParseError (runParseError), runParser)
import System.Console.Readline

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
runAndEval m s =
  runMaybeT
    ( do
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
    )
    >>= maybe (return m) return

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "This program can evaluate some simple expression",
        "Only integer number are supported",
        "The supported operations are +,-,/,^",
        "You can also assignate values to variables, using the syntax `variable = value` ",
        "Debug features:",
        "You can show some internal structures by providing a debug flag",
        "such debug flag is ':c' where c is a character which will dictate the kind of debug",
        ":a to see ast, :t to see the tokens, :h to print this help, :m to see the stored variables values"
      ]

getDebugInfo :: [SToken] -> ([SToken], Char)
getDebugInfo (TokDebug d : xs) = (xs, d)
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
