module Main where

import           Myml.Parser
import           Myml.Syntax
import           Data.Text.Prettyprint.Doc
import           Text.Trifecta
import           System.Console.Haskeline
import           Control.Monad.Trans
import           System.Console.ANSI
import           System.Directory
import           System.FilePath

historyFileName :: FilePath
historyFileName = ".myml_history"

prompt :: String
prompt = "myml> "

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let haskelineSettings = Settings
        { complete       = completeFilename
        , historyFile    = Just $ homeDir </> historyFileName
        , autoAddHistory = True
        }
  runInputT haskelineSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine prompt
  case minput of
    Nothing          -> return ()
    Just (':' : cmd) -> command cmd
    Just input       -> do
      let parser      = parseTerm <* eof
      let parseResult = parseString parser mempty input
      case parseResult of
        Failure errInfo -> outputStrLn $ show (_errDoc errInfo)
        Success term    -> lift (processTerm term)
      loop

command :: String -> InputT IO ()
command "q"    = command "exit"
command "quit" = command "exit"
command "exit" = return ()
command ""     = command "help"
command "?"    = command "help"
command "h"    = command "help"
command "help" = lift outputHelp >> loop
command cmd    = do
  lift $ commandErrorLabel "error"
  lift $ putStrLn ("unknown command ':" ++ cmd ++ "'")
  loop

outputHelp :: IO ()
outputHelp = putStrLn "help unimplemented"

processTerm :: Term -> IO ()
processTerm input = print (pretty input)

commandErrorLabel :: String -> IO ()
commandErrorLabel label = bold (withColor Dull Red (putStr (label ++ ": ")))

bold :: IO () -> IO ()
bold = withSGR [SetConsoleIntensity BoldIntensity]

withColor :: ColorIntensity -> Color -> IO () -> IO ()
withColor intensity color = withSGR [SetColor Foreground intensity color]

withSGR :: [SGR] -> IO () -> IO ()
withSGR sgr io = do
  setSGR sgr
  io
  setSGR [Reset]
