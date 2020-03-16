module Myml.Parser.Helper
  ( chainedPrefix
  , chainedPostfix
  )
where

import           Text.Trifecta

chainedPrefix :: Parser (a -> a) -> Parser (a -> a)
chainedPrefix p = chainl1 p (return (.))

chainedPostfix :: Parser (a -> a) -> Parser (a -> a)
chainedPostfix p = chainr1 p (return $ flip (.))
