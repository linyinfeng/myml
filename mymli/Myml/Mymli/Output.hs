module Myml.Mymli.Output
  ( commandErrorLabel
  , bold
  , withColor
  , withSGR
  )
where

import           System.Console.ANSI

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
