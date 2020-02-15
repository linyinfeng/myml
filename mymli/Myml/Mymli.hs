module Myml.Mymli
  ( main
  )
where

import           Myml.Parser
import           Myml.Syntax
import           Myml.Eval
import           Myml.Mymli.Environment
import           Data.Text.Prettyprint.Doc
import           Text.Trifecta
import           System.Console.Haskeline
import           Control.Monad.State
import           System.Console.ANSI
import           System.Directory
import           System.FilePath

historyFileName :: FilePath
historyFileName = ".myml_history"

prompt :: String
prompt = "mymli> "

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let haskelineSettings = Settings
        { complete       = completeFilename
        , historyFile    = Just $ homeDir </> historyFileName
        , autoAddHistory = True
        }
  runInputT haskelineSettings (evalStateT loop initialRuntimeEnvironment)

loop :: StateT RuntimeEnvironment (InputT IO) ()
loop = do
  minput <- lift (getInputLine prompt)
  case minput of
    Nothing          -> return ()
    Just (':' : cmd) -> command cmd
    Just input       -> do
      let parser      = parseTerm <* eof
      let parseResult = parseString parser mempty input
      case parseResult of
        Failure errInfo -> lift (outputStrLn (show (_errDoc errInfo)))
        Success term    -> processTerm term
      loop

command :: String -> StateT RuntimeEnvironment (InputT IO) ()
command "q"    = command "exit"
command "quit" = command "exit"
command "exit" = return ()
command ""     = command "help"
command "?"    = command "help"
command "h"    = command "help"
command "help" = lift outputHelp >> loop
command cmd    = do
  lift $ commandErrorLabel "error"
  lift $ outputStrLn ("unknown command ':" ++ cmd ++ "'")
  loop

outputHelp :: InputT IO ()
outputHelp = outputStrLn "help unimplemented"

processTerm :: Term -> StateT RuntimeEnvironment (InputT IO) ()
processTerm input = do
  store <- gets envStore
  let (evaled, store') = runState (bigStep input) store
  modify (\env -> env { envStore = store' })
  lift (outputStrLn (show (pretty evaled)))

commandErrorLabel :: String -> InputT IO ()
commandErrorLabel label = bold (withColor Dull Red (outputStr (label ++ ": ")))

bold :: InputT IO () -> InputT IO ()
bold = withSGR [SetConsoleIntensity BoldIntensity]

withColor :: ColorIntensity -> Color -> InputT IO () -> InputT IO ()
withColor intensity color = withSGR [SetColor Foreground intensity color]

withSGR :: [SGR] -> InputT IO () -> InputT IO ()
withSGR sgr io = do
  lift (setSGR sgr)
  io
  lift (setSGR [Reset])
