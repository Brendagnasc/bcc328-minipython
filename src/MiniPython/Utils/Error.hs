-- | Erro posicionado, comum ao analisador lexico e aos tres parsers.
module MiniPython.Utils.Error
  ( Error (..)
  , formatError
  ) where

import MiniPython.Frontend.Lexer.Token (Pos (..))

data Error = Error
  { errPos :: Pos
  , errMsg :: String
  }
  deriving (Eq, Ord)

-- | Formato da secao 2.1 do enunciado:
-- @arquivo:linha:coluna: error: mensagem@.
formatError :: FilePath -> Error -> String
formatError file e =
  file
    ++ ":" ++ show (posLine (errPos e))
    ++ ":" ++ show (posCol (errPos e))
    ++ ": error: " ++ errMsg e
