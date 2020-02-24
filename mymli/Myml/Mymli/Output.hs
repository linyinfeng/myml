module Myml.Mymli.Output
  ( errorLabel
  , typingErrorLabel
  , bold
  , withColor
  , withSGR
  )
where

import           System.Console.ANSI

typingErrorLabel :: IO ()
typingErrorLabel = errorLabel "Typing Error"

errorLabel :: String -> IO ()
errorLabel label = bold (withColor Dull Red (putStr (label ++ ": ")))

bold :: IO () -> IO ()
bold = withSGR [SetConsoleIntensity BoldIntensity]

withColor :: ColorIntensity -> Color -> IO () -> IO ()
withColor intensity color = withSGR [SetColor Foreground intensity color]

withSGR :: [SGR] -> IO () -> IO ()
withSGR sgr io = do
  setSGR sgr
  io
  setSGR [Reset]
