module Myml.Mymli
  ( main
  )
where

import           Myml.Mymli.Environment
import           Myml.Mymli.Input
import           Myml.Mymli.Text
import           Myml.Mymli.Option
import Text.Trifecta
import           Control.Monad.State
import qualified Options.Applicative           as O
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text.IO                  as Text.IO
import           System.Console.Haskeline
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
  options <- liftIO (O.execParser optsInfo)
  let env = emptyMymlEnv options
  runInputT haskelineSettings (evalMymli (greeting >> loop >> bye) env)
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
    Left info  -> liftIO (print (_errDoc info)) >> handleMymliRequest MymliContinue
    Right input -> processInput input >>= handleMymliRequest

handleMymliRequest :: MymliRequest -> Mymli (InputT IO) ()
handleMymliRequest MymliContinue = loop
handleMymliRequest MymliExit     = return ()
