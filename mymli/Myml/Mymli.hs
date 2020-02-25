module Myml.Mymli
  ( main
  )
where

import           Myml.Mymli.Environment
import           Myml.Mymli.Common
import           Myml.Mymli.Command
import           Myml.Mymli.Input
import           Myml.Mymli.Command.Parser
import           Myml.Mymli.Input.Parser
import           Myml.Mymli.Text
import qualified Data.Text.IO                  as Text.IO
import           Text.Trifecta
import           System.Console.Haskeline
import           Control.Monad.State
import           System.Directory
import           System.FilePath

historyFileName :: FilePath
historyFileName = ".myml_history"

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let haskelineSettings = Settings
        { complete       = completeFilename
        , historyFile    = Just $ homeDir </> historyFileName
        , autoAddHistory = True
        }
  runInputT haskelineSettings (evalMymli (greeting >> loop >> bye) emptyMymlEnv)

greeting :: Mymli (InputT IO) ()
greeting = liftIO (Text.IO.putStrLn mymliGreetingText)

bye :: Mymli (InputT IO) ()
bye = liftIO (Text.IO.putStrLn mymliByeText)

loop :: Mymli (InputT IO) ()
loop = do
  minput <- lift (getInputLine prompt)
  case minput of
    Nothing                -> return ()
    Just (':' : cmdString) -> do
      res <- liftIO
        (parseAndPrintError (whiteSpace *> parseCommand <* eof) cmdString)
      case res of
        Nothing  -> handleMymliRequest MymliContinue
        Just cmd -> processCommand cmd >>= handleMymliRequest
    Just inputString -> do
      res <- liftIO
        (parseAndPrintError (whiteSpace *> parseInput <* eof) inputString)
      case res of
        Nothing    -> handleMymliRequest MymliContinue
        Just input -> processInput input >>= handleMymliRequest

handleMymliRequest :: MymliRequest -> Mymli (InputT IO) ()
handleMymliRequest MymliContinue = loop
handleMymliRequest MymliExit     = return ()
