module Myml.Mymli
  ( main
  )
where

import           Myml.Mymli.Environment
import           Myml.Mymli.Parser
import           Myml.Mymli.Command
import           Myml.Mymli.Input
import           Text.Trifecta
import           System.Console.Haskeline
import           Control.Monad.State
import           System.Directory
import           System.FilePath

historyFileName :: FilePath
historyFileName = ".myml_history"

prompt :: String
prompt = "\ESC[1;32m\STXmymli\ESC[0m\STX> "

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let haskelineSettings = Settings
        { complete       = completeFilename
        , historyFile    = Just $ homeDir </> historyFileName
        , autoAddHistory = True
        }
  runInputT haskelineSettings (evalMymli loop emptyMymlEnv)

loop :: Mymli (InputT IO) ()
loop = do
  minput <- lift (getInputLine prompt)
  case minput of
    Nothing                -> return ()
    Just (':' : cmdString) -> do
      let parser = parseCommand <* eof
      let result = runParser parser mempty cmdString
      case result of
        Failure (ErrInfo d _) ->
          liftIO (print d) >> handleMymliRequest MymliContinue
        Success cmd -> processCommand cmd >>= handleMymliRequest
    Just inputString -> do
      let parser = parseInput <* eof
      let result = runParser parser mempty inputString
      case result of
        Failure (ErrInfo d _) ->
          liftIO (print d) >> handleMymliRequest MymliContinue
        Success input -> processInput input >>= handleMymliRequest

handleMymliRequest :: MymliRequest -> Mymli (InputT IO) ()
handleMymliRequest MymliContinue = loop
handleMymliRequest MymliExit     = return ()
