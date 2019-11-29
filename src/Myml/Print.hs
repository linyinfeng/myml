module Myml.Print
  ( SyntaxPrint(..)
  )
where

import           Myml.Syntax
import           System.Console.ANSI

class SyntaxPrint a where
  sPrint :: a -> IO ()
  sPrintLn :: a -> IO ()
  sPrintLn t = sPrint t >> putStrLn ""

instance SyntaxPrint Term where
  sPrint (TmVar x) = putVar x
  sPrint (TmApp t1 t2) =
    putStr "(" >> sPrint t1 >> putStr " " >> sPrint t2 >> putStr ")"
  sPrint (TmAbs x ty t2) =
    putStr "("
      >> putBuiltInSyntax "λ"
      >> putVar x
      >> putBuiltInSyntax ":"
      >> putStr " "
      >> sPrint ty
      >> putBuiltInSyntax "."
      >> putStr " "
      >> sPrint t2
      >> putStr ")"
  sPrint TmTrue  = putBuiltInTerm "true"
  sPrint TmFalse = putBuiltInTerm "false"
  sPrint (TmIf t1 t2 t3) =
    putStr "("
      >> putBuiltInSyntax "if"
      >> putStr " "
      >> sPrint t1
      >> putStr " "
      >> putBuiltInSyntax "then"
      >> putStr " "
      >> sPrint t2
      >> putStr " "
      >> putBuiltInSyntax "else"
      >> putStr " "
      >> sPrint t3
      >> putStr ")"
  sPrint TmZero = putBuiltInTerm "0"
  sPrint (TmSucc n) =
    putStr "(" >> putBuiltInFunc "succ" >> putStr " " >> sPrint n >> putStr ")"
  sPrint (TmPred n) =
    putStr "(" >> putBuiltInFunc "pred" >> putStr " " >> sPrint n >> putStr ")"
  sPrint (TmIsZero n) =
    putStr "(" >> putBuiltInFunc "iszero" >> putStr " " >> sPrint n >> putStr
      ")"
  sPrint TmUnit = putBuiltInTerm "unit"

instance SyntaxPrint Type where
  sPrint TyBool = putBuiltInType "Bool"
  sPrint TyNat  = putBuiltInType "Nat"
  sPrint TyUnit = putBuiltInType "Unit"
  sPrint (TyFunc ty1 ty2) =
    putStr "("
      >> sPrint ty1
      >> putStr " "
      >> putBuiltInSyntax "->"
      >> putStr " "
      >> sPrint ty2
      >> putStr ")"
  sPrint (TyVar tyx) = putTypeVar tyx

putWithSGR :: [SGR] -> String -> IO ()
putWithSGR sgr s = do
  -- setSGR [SetConsoleIntensity BoldIntensity]
  setSGR sgr
  putStr s
  setSGR [Reset]

putBuiltInType :: String -> IO ()
putBuiltInType = putWithSGR [SetColor Foreground Dull Blue]

putBuiltInTerm :: String -> IO ()
putBuiltInTerm = putWithSGR [SetColor Foreground Dull Black]

putBuiltInFunc :: String -> IO ()
putBuiltInFunc = putWithSGR [SetColor Foreground Dull Black]

putBuiltInSyntax :: String -> IO ()
putBuiltInSyntax = putWithSGR [SetColor Foreground Dull Magenta]

putTypeVar :: String -> IO ()
putTypeVar =
  putWithSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Blue]

putVar :: String -> IO ()
putVar =
  putWithSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Black]
