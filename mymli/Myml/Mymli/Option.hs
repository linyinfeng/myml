module Myml.Mymli.Option
  ( MymliOptions(..)
  , mymliOptions
  )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

newtype MymliOptions = MymliOptions
  { optImperativeFeaturesEnabled :: Bool }
  deriving (Show)

mymliOptions :: Parser MymliOptions
mymliOptions = MymliOptions <$> switch
  (long "enable-imperative-features" <> short 'i' <> help
    "Whether to enable imperative features"
  )
