module Myml.Mymli.Command
  ( Command(..)
  , processCommand
  )
where

import           Myml.Mymli.Environment
import           Myml.Syntax
import           Myml.Mymli.Common
import           Control.Monad.Trans
import           Data.Text.Prettyprint.Doc

data Command = CmdExit
             | CmdHelp
             | CmdShowType Term
             deriving (Show)

processCommand :: MonadIO m => Command -> Mymli m MymliRequest
processCommand CmdExit = return MymliExit
processCommand CmdHelp = do
  liftIO (putStrLn "Help is unimplemented")
  return MymliContinue
processCommand (CmdShowType t) = do
  t' <- mymliAddLets t
  case mymliInferType t' of
    Left  e -> liftIO (putStrLn ("[Typing Error] " ++ show e))
    Right s -> liftIO (print (pretty s))
  return MymliContinue
