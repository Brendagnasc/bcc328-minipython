{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- A instancia de Stream e declarada aqui porque nem a classe (de
-- megaparsec) nem o tipo Token (do nosso lexer) pertencem a este
-- modulo. Nao ha outra implementacao possivel para esse par, de modo
-- que o risco associado a instancias orfas nao se aplica.

-- | Adaptacao da lista de tokens ao fluxo de entrada do Megaparsec.
--
-- O parser descendente recursivo opera sobre a saida do analisador
-- lexico, ja com INDENT e DEDENT inseridos, e nao sobre o texto fonte.
-- Com isso os blocos sao delimitados por tokens e nao ha necessidade de
-- combinadores de indentacao na camada sintatica.
--
-- Megaparsec define Token com o mesmo nome do nosso lexer, por isso o
-- modulo do lexer e importado qualificado como L.
module MiniPython.Frontend.Parser.Stream
  ( TokenStream (..)
  , toStream
  ) where

import Data.Proxy (Proxy (..))
import qualified MiniPython.Frontend.Lexer.Token as L
import Text.Megaparsec (Stream (..))

-- | Fluxo de tokens consumido pelo parser.
newtype TokenStream = TokenStream { unTokenStream :: [L.Token] }
  deriving (Eq, Ord)

toStream :: [L.Token] -> TokenStream
toStream = TokenStream

instance Stream TokenStream where
  type Token TokenStream = L.Token
  type Tokens TokenStream = [L.Token]

  tokenToChunk Proxy t = [t]
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null

  take1_ (TokenStream []) = Nothing
  take1_ (TokenStream (t : ts)) = Just (t, TokenStream ts)

  takeN_ n s@(TokenStream ts)
    | n <= 0 = Just ([], s)
    | null ts = Nothing
    | otherwise = let (xs, ys) = splitAt n ts in Just (xs, TokenStream ys)

  takeWhile_ f (TokenStream ts) =
    let (xs, ys) = span f ts in (xs, TokenStream ys)
