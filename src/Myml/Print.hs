module Myml.Print
  ( printTerm
  , printTermLn
  , printType
  , printTypeLn
  )
where

import           Myml.Syntax
import           System.Console.ANSI

printTermLn :: Term -> IO ()
printTermLn t = printTerm t >> putStrLn ""

printTypeLn :: Type -> IO ()
printTypeLn ty = printType ty >> putStrLn ""

printTerm :: Term -> IO ()
printTerm (TmVar x) = putVar x
printTerm (TmApp t1 t2) =
  putStr "(" >> printTerm t1 >> putStr " " >> printTerm t2 >> putStr ")"
printTerm (TmAbs x ty t2) =
  putStr "("
    >> putBuiltInSyntax "λ"
    >> putVar x
    >> putBuiltInSyntax ":"
    >> putStr " "
    >> printType ty
    >> putBuiltInSyntax "."
    >> putStr " "
    >> printTerm t2
    >> putStr ")"
printTerm TmTrue  = putBuiltInTerm "true"
printTerm TmFalse = putBuiltInTerm "false"
printTerm (TmIf t1 t2 t3) =
  putStr "("
    >> putBuiltInSyntax "if"
    >> putStr " "
    >> printTerm t1
    >> putStr " "
    >> putBuiltInSyntax "then"
    >> putStr " "
    >> printTerm t2
    >> putStr " "
    >> putBuiltInSyntax "else"
    >> putStr " "
    >> printTerm t3
    >> putStr ")"
printTerm TmZero = putBuiltInTerm "0"
printTerm (TmSucc n) =
  putStr "(" >> putBuiltInFunc "succ" >> putStr " " >> printTerm n >> putStr ")"
printTerm (TmPred n) =
  putStr "(" >> putBuiltInFunc "pred" >> putStr " " >> printTerm n >> putStr ")"
printTerm (TmIsZero n) =
  putStr "(" >> putBuiltInFunc "iszero" >> putStr " " >> printTerm n >> putStr
    ")"
printTerm TmUnit = putBuiltInTerm "unit"

printType :: Type -> IO ()
printType TyBool = putBuiltInType "Bool"
printType TyNat  = putBuiltInType "Nat"
printType TyUnit = putBuiltInType "Unit"
printType (TyFunc ty1 ty2) =
  putStr "("
    >> printType ty1
    >> putStr " "
    >> putBuiltInSyntax "->"
    >> putStr " "
    >> printType ty2
    >> putStr ")"
printType (TyVar tyx) = putTypeVar tyx

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
