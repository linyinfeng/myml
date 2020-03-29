module Myml.Mymli.Option
  ( MymliOptions(..)
  , mymliOptions
  )
where

import           Options.Applicative

newtype MymliOptions = MymliOptions
  { optPure :: Bool }
  deriving (Show)

mymliOptions :: Parser MymliOptions
mymliOptions = MymliOptions <$> switch
  (long "pure" <> short 'p' <> help "Whether to disable imperative features")
