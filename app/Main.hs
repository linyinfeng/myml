module Main where

import           Myml.Parser
import           Myml.Eval
import           Myml.Syntax
import           Myml.Print
import           Text.Trifecta
import           System.Console.Haskeline
import           Control.Monad.Trans
import           System.Console.ANSI

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "myml> "
    case minput of
      Nothing    -> return ()
      Just ":q"  -> return ()
      Just input -> do
        let parser      = parseTerm <* eof
        let parseResult = parseString parser mempty input
        case parseResult of
          Failure errInfo -> outputStrLn $ show (_errDoc errInfo)
          Success term    -> lift (processTerm term)
        loop

processTerm :: Term -> IO ()
processTerm term = do
  outputLabel Blue "debug"
  print term
  outputLabel Green "input"
  printTermLn term
  let evaluated = eval term
  if isValue evaluated
    then do
      outputLabel Green "value"
      printTermLn evaluated
    else do
      outputLabel Red "stuck"
      printTermLn evaluated

outputLabel :: Color -> String -> IO ()
outputLabel color label = do
  setSGR [SetColor Foreground Dull color]
  putStr label
  setSGR [Reset]
  putStr ": "
