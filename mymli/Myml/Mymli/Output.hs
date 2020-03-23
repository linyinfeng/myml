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
  ("fatal error: displayValue called with non-value term : " ++ show (pretty t))

displayValue :: Term -> Doc ann
displayValue = displayValuePrec 0

displayValuePrec :: Int -> Term -> Doc ann
displayValuePrec _ t | not (isValue t)                    = displayValueErr t
displayValuePrec _ rv@(TmApp (TmApp (TmRcdExtend _) _) _) = displayRcdValue rv
displayValuePrec _ rv@TmEmptyRcd                          = displayRcdValue rv
displayValuePrec _ mv@(TmApp (TmApp (TmMatchExtend _) _) _) =
  displayMatchValue mv
displayValuePrec _ mv@TmEmptyMatch = displayMatchValue mv
displayValuePrec n (TmApp v1 v2)   = parensPrec
  (n > prec)
  (align
    (displayValuePrec prec v1 <> softline <> displayValuePrec (prec + 1) v2)
  )
  where prec = 1
displayValuePrec _ TmAbs{} = pretty "<\x3bb>"
displayValuePrec n t       = prettyPrec n t

displayRcdValue :: Term -> Doc ann
displayRcdValue t = align (group (open <> display True t <> close))
 where
  open  = flatAlt (pretty "{ ") (pretty "{")
  close = flatAlt (pretty " }") (pretty "}")
  display lastExtend (TmApp (TmApp (TmRcdExtend l) v) rv) =
    display False rv
      <>  pretty l
      <+> pretty "="
      <+> displayValue v
      <>  (if lastExtend then mempty else line <> pretty ", ")
  display _ TmEmptyRcd = mempty
  display _ t'         = displayValueErr t'

displayMatchValue :: Term -> Doc ann
displayMatchValue t = align (group (open <> display True t <> close))
 where
  open  = flatAlt (pretty "[ ") (pretty "[")
  close = flatAlt (pretty " ]") (pretty "]")
  display lastExtend (TmApp (TmApp (TmMatchExtend l) v) rv) =
    display False rv
      <>  prettyVariantLabel l
      <+> pretty "="
      <+> displayValue v
      <>  (if lastExtend then mempty else line <> pretty ", ")
  display _ TmEmptyMatch = mempty
  display _ t'           = displayValueErr t'

displayScheme :: TypeScheme -> Doc ann
displayScheme (ScmForall _ _ t) = displayScheme t
displayScheme (ScmMono t      ) = pretty t
