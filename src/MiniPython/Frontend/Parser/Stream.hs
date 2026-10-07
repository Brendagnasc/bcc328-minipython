{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- A instancia de Stream e declarada aqui porque nem a classe (de
-- megaparsec) nem o tipo Token (do nosso lexer) pertencem a este
-- modulo. Nao ha outra implementacao possivel para esse par, de modo
-- que o risco associado a instancias orfas nao se aplica.

-- | Adaptacao da lista de tokens ao fluxo de entrada do Megaparsec.
--
-- O parser PEG opera sobre a saida do analisador lexico, ja com
-- INDENT e DEDENT inseridos, e nao sobre o texto fonte. Com isso os
-- blocos sao delimitados por tokens e nao ha necessidade de
-- combinadores de indentacao na camada sintatica.
--
-- Megaparsec define Token e Pos com os mesmos nomes do nosso lexer,
-- por isso o modulo do lexer e importado qualificado como L.
module MiniPython.Frontend.Parser.Stream
  ( TokenStream (..)
  , PegParser
  , toStream
  , posFromSourcePos
  , sourcePosFromPos
  ) where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import qualified MiniPython.Frontend.Lexer.Token as L
import Text.Megaparsec

-- | Fluxo de tokens consumido pelo parser PEG.
newtype TokenStream = TokenStream { unTokenStream :: [L.Token] }
  deriving (Eq, Ord)

-- | Tipo concreto dos parsers do analisador PEG.
type PegParser = Parsec Void TokenStream

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

instance VisualStream TokenStream where
  showTokens Proxy = intercalate ", " . NE.toList . fmap showToken
  tokensLength Proxy = length

-- | O Megaparsec usa esta instancia para localizar o erro no fonte.
-- Como o fluxo e de tokens, a posicao vem do proprio token em vez de
-- ser obtida contando caracteres do texto original.
instance TraversableStream TokenStream where
  reachOffset o pst =
    ( Nothing
    , pst
        { pstateOffset = max (pstateOffset pst) o
        , pstateSourcePos = posFor resto
        , pstateInput = TokenStream resto
        }
    )
    where
      toks = unTokenStream (pstateInput pst)
      resto = drop (o - pstateOffset pst) toks
      posFor (t : _) =
        sourcePosFromPos (sourceName (pstateSourcePos pst)) (L.tokPos t)
      posFor [] = pstateSourcePos pst

showToken :: L.Token -> String
showToken t = case L.tokLexeme t of
  L.TIdent n     -> "identificador " ++ show n
  L.TIntLit n    -> "literal inteiro " ++ show n
  L.TFloatLit d  -> "literal float " ++ show d
  L.TStringLit s -> "literal string " ++ show s
  L.TBoolLit b   -> if b then "True" else "False"
  L.TNewline     -> "fim de linha"
  L.TIndent      -> "aumento de indentacao"
  L.TDedent      -> "fim de bloco"
  L.TEOF         -> "fim do arquivo"
  other          -> L.describeLexeme other

-- | Converte a posicao do nosso lexer para a do Megaparsec.
sourcePosFromPos :: FilePath -> L.Pos -> SourcePos
sourcePosFromPos file p =
  SourcePos file (mkPos (L.posLine p)) (mkPos (L.posCol p))

-- | Converte de volta, para que os erros do parser PEG usem o mesmo
-- formato dos demais analisadores.
posFromSourcePos :: SourcePos -> L.Pos
posFromSourcePos sp =
  L.Pos (unPos (sourceLine sp)) (unPos (sourceColumn sp))
