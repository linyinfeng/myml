module Myml.Parser.Helper
  ( chainedPrefix,
    chainedPostfix,
  )
where

import Myml.Parser.Common
import Text.Trifecta hiding (Parser)

chainedPrefix :: Parser (a -> a) -> Parser (a -> a)
chainedPrefix p = chainl1 p (return (.))

chainedPostfix :: Parser (a -> a) -> Parser (a -> a)
chainedPostfix p = chainr1 p (return $ flip (.))
