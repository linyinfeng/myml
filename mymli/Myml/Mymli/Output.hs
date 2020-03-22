module Myml.Mymli.Output
  ( ioErrorLabel
  , errorLabel
  , typingErrorLabel
  , bold
  , withColor
  , withSGR
  , displayValue
  , displayScheme
  )
where

import           Myml.Syntax
import           System.Console.ANSI
import           Data.Text.Prettyprint.Doc

ioErrorLabel :: IO ()
ioErrorLabel = errorLabel "IO Error"

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

displayValueErr :: Term -> a
displayValueErr t = error
  (  "fatal error: displayValue called with non-value term : "
  ++ (show (pretty t))
  )

displayValue :: Term -> Doc ann
displayValue rv@(TmApp (TmApp (TmRcdExtend _) _) _)   = displayRcdValue rv
displayValue rv@TmEmptyRcd                            = displayRcdValue rv
displayValue mv@(TmApp (TmApp (TmMatchExtend _) _) _) = displayMatchValue mv
displayValue mv@TmEmptyMatch                          = displayMatchValue mv
displayValue TmAbs{}                                  = pretty "<\x3bb>"
displayValue t | isValue t = pretty t
displayValue t             = displayValueErr t

displayRcdValue :: Term -> Doc ann
displayRcdValue t = align (group (open <> display True t <> close))
 where
  open  = flatAlt (pretty "{ ") (pretty "{")
  close = flatAlt (pretty " }") (pretty "}")
  display first (TmApp (TmApp (TmRcdExtend l) v) rv) =
    (if first then mempty else line <> pretty ", ")
      <>  pretty l
      <+> pretty "="
      <+> displayValue v
      <>  display False rv
  display _ TmEmptyRcd = mempty
  display _ t'         = displayValueErr t'

displayMatchValue :: Term -> Doc ann
displayMatchValue t = align (group (open <> display True t <> close))
 where
  open  = flatAlt (pretty "[ ") (pretty "[")
  close = flatAlt (pretty " ]") (pretty "]")
  display first (TmApp (TmApp (TmMatchExtend l) v) rv) =
    (if first then mempty else line <> pretty ", ")
      <>  prettyVariantLabel l
      <+> pretty "="
      <+> displayValue v
      <>  display False rv
  display _ TmEmptyMatch = mempty
  display _ t'           = displayValueErr t'

displayScheme :: TypeScheme -> Doc ann
displayScheme (ScmForall _ _ t) = displayScheme t
displayScheme (ScmMono t      ) = pretty t
