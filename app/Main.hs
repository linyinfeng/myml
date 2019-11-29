module Main where

import           Myml.Parser
import           Myml.Eval
import           Myml.Syntax
import           Myml.Print
import           Myml.Typing
import           Text.Trifecta
import           System.Console.Haskeline
import           Control.Monad.Trans
import           System.Console.ANSI
import           System.Directory
import           System.FilePath

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let historyFileName = ".myml_history"
  let haskelineSettings = Settings
        { complete       = completeFilename
        , historyFile    = Just $ homeDir </> historyFileName
        , autoAddHistory = True
        }
  runInputT haskelineSettings loop
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
  sPrintLn term
  let evaluated = eval term
  if isValue evaluated
    then do
      outputLabel Green "value"
      sPrintLn evaluated
    else do
      outputLabel Red "stuck"
      sPrintLn evaluated
  let globalCtx   = []
  let constraints = genTyCons globalCtx freeTyVar term
  case constraints of
    Left e -> do
      outputLabel Red "gen constraints faild"
      print e
    Right (ty, _f, c) -> do
      outputLabel Green "constraints"
      print c
      outputLabel Green "unsolved type"
      sPrintLn ty
      case unifyCons c of
        Left e -> do
          outputLabel Red "failed to unify constraints"
          print e
        Right solution -> do
          outputLabel Green "solution"
          print solution
          let solved = applyTySub solution ty
          outputLabel Green "solved type"
          sPrintLn solved
          let solvedTerm = applyTySubToTerm solution term
          outputLabel Green "explicitly typed"
          sPrintLn solvedTerm

outputLabel :: Color -> String -> IO ()
outputLabel color label = do
  setSGR [SetColor Foreground Dull color]
  putStr label
  setSGR [Reset]
  putStr ": "
