module Myml.Mymli
  ( main
  )
where

import           Myml.Mymli.Environment
import           Myml.Mymli.Input
import           Myml.Mymli.Text
import           Myml.Mymli.Option
import           Myml.Mymli.Lang
import           Text.Trifecta
import           Control.Monad.State
import qualified Options.Applicative           as O
import qualified Data.Text.IO                  as Text.IO
import           System.Console.Haskeline
import           System.Directory
import           System.FilePath
import           System.Exit

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
  options <- liftIO (O.execParser optsInfo)
  let env = emptyMymlEnv options
  case optFile options of
    Nothing ->
      runInputT haskelineSettings (evalMymli (greeting >> loop >> bye) env)
    Just file -> (boolToExitCode <$> evalMymli (runFile file) env) >>= exitWith
 where
  optsInfo = O.info
    (mymliOptions O.<**> O.helper)
    (  O.fullDesc
    <> O.progDesc "Simple REPL for the language myml"
    <> O.header "mymli -- myml's interactive environment"
    <> O.header mymlOptionHelpHeaderString
    )

greeting :: Mymli (InputT IO) ()
greeting = liftIO (Text.IO.putStrLn mymliGreetingText)

bye :: Mymli (InputT IO) ()
bye = liftIO (Text.IO.putStrLn mymliByeText)

loop :: Mymli (InputT IO) ()
loop = do
  res <- lift getMymliInput
  case res of
    Left info ->
      liftIO (print (_errDoc info)) >> handleMymliRequest MymliContinue
    Right input -> processInput input >>= handleMymliRequest

runFile :: FilePath -> Mymli IO Bool
runFile path = do
  result <- searchAndParseFile path
  case result of
    Nothing          -> return False
    Just (inputs, _) -> do
      env <- mymliEnvForFile path
      liftIO (evalMymli (processTopLevels True inputs) env)

boolToExitCode :: Bool -> ExitCode
boolToExitCode True  = ExitSuccess
boolToExitCode False = ExitFailure 1

handleMymliRequest :: MymliRequest -> Mymli (InputT IO) ()
handleMymliRequest MymliContinue = loop
handleMymliRequest MymliExit     = return ()
