module Myml.Mymli.Option
  ( MymliOptions(..)
  , mymliOptions
  )
where

import           Options.Applicative

data MymliOptions = MymliOptions
  { optFile :: Maybe FilePath
  , optPure :: Bool
  , optVerbose :: Bool}
  deriving (Show)

mymliOptions :: Parser MymliOptions
mymliOptions =
  MymliOptions
    <$> optional (argument str (metavar "FILE"))
    <*> switch
          (long "pure" <> short 'p' <> help
            "Whether to disable imperative features"
          )
    <*> switch
          (long "verbose" <> short 'v' <> help
            "Whether to print in verbose mode"
          )
