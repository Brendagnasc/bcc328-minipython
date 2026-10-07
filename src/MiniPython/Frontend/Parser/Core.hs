-- | Mecanismo basico do analisador descendente recursivo.
--
-- Um valor do tipo @Parser a@ e uma funcao que consome um prefixo da
-- lista de tokens e devolve um valor de tipo @a@ junto com os tokens
-- restantes, ou um erro posicionado. As funcoes deste modulo cuidam
-- apenas da mecanica de avancar e falhar; as regras da gramatica
-- ficam no modulo RecursiveDescent.
module MiniPython.Frontend.Parser.Core
  ( Parser (..)
  , ParseError (..)
  , formatParseError
  , runFully
  , peek
  , peekAt
  , advance
  , expect
  , expectIdent
  , check
  , accept
  , failWith
  , failAt
  , many0
  , sepBy1
  ) where

import Control.Applicative (Alternative (..))
import MiniPython.Frontend.Lexer.Token

-- | Erro sintatico: posicao do token inesperado, o que se esperava e
-- a producao em analise, conforme exigido na secao 3 do enunciado.
data ParseError = ParseError
  { pePos      :: Pos
  , peFound    :: String
  , peExpected :: String
  , peContext  :: String
  }

formatParseError :: FilePath -> ParseError -> String
formatParseError file e =
  file
    ++ ":" ++ show (posLine (pePos e))
    ++ ":" ++ show (posCol (pePos e))
    ++ ": erro sintatico: esperado " ++ peExpected e
    ++ ", encontrado " ++ peFound e
    ++ " (em " ++ peContext e ++ ")"

newtype Parser a = Parser
  { runParser :: [Token] -> Either ParseError (a, [Token])
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \ts -> do
    (a, ts') <- p ts
    pure (f a, ts')

instance Applicative Parser where
  pure a = Parser $ \ts -> Right (a, ts)
  Parser pf <*> Parser pa = Parser $ \ts -> do
    (f, ts1) <- pf ts
    (a, ts2) <- pa ts1
    pure (f a, ts2)

instance Monad Parser where
  Parser p >>= f = Parser $ \ts -> do
    (a, ts1) <- p ts
    runParser (f a) ts1

-- | Alternativa com retrocesso total: se o primeiro parser falha, o
-- segundo recomeca da mesma posicao. Usada apenas onde a decisao nao
-- pode ser tomada olhando um unico token.
instance Alternative Parser where
  empty = Parser $ \ts -> Left (unexpectedAt ts "nenhuma alternativa" "alternativa")
  Parser p <|> Parser q = Parser $ \ts -> case p ts of
    Right r -> Right r
    Left _  -> q ts

-- | Executa o parser exigindo que toda a entrada seja consumida ate EOF.
runFully :: Parser a -> [Token] -> Either ParseError a
runFully p ts = do
  (a, rest) <- runParser p ts
  case rest of
    (t : _) | tokLexeme t /= TEOF ->
      Left (ParseError (tokPos t) (showLexeme (tokLexeme t)) "fim do arquivo" "programa")
    _ -> pure a

-- | Proximo token sem consumi-lo.
peek :: Parser Token
peek = Parser $ \ts -> case ts of
  (t : _) -> Right (t, ts)
  []      -> Left (ParseError initialPos "fim da entrada" "um token" "programa")

-- | Token n posicoes a frente, sem consumir. Usado nos poucos pontos
-- em que um unico token nao basta para decidir a producao.
peekAt :: Int -> Parser (Maybe Token)
peekAt n = Parser $ \ts -> Right (listToMaybe (drop n ts), ts)
  where
    listToMaybe (x : _) = Just x
    listToMaybe []      = Nothing

-- | Consome e devolve o proximo token.
advance :: Parser Token
advance = Parser $ \ts -> case ts of
  (t : rest) -> Right (t, rest)
  []         -> Left (ParseError initialPos "fim da entrada" "um token" "programa")

-- | Consome o proximo token exigindo que seja o lexema dado.
-- O terceiro argumento nomeia a producao em analise, que aparece na
-- mensagem de erro.
expect :: Lexeme -> String -> String -> Parser Token
expect lx desc ctx = Parser $ \ts -> case ts of
  (t : rest)
    | tokLexeme t == lx -> Right (t, rest)
    | otherwise ->
        Left (ParseError (tokPos t) (showLexeme (tokLexeme t)) desc ctx)
  [] -> Left (ParseError initialPos "fim da entrada" desc ctx)

-- | Consome um identificador, devolvendo o nome e a posicao.
expectIdent :: String -> Parser (Pos, Name)
expectIdent ctx = Parser $ \ts -> case ts of
  (t : rest) -> case tokLexeme t of
    TIdent n -> Right ((tokPos t, n), rest)
    other    -> Left (ParseError (tokPos t) (showLexeme other) "um identificador" ctx)
  [] -> Left (ParseError initialPos "fim da entrada" "um identificador" ctx)

-- | Verifica se o proximo token e o lexema dado, sem consumir.
check :: Lexeme -> Parser Bool
check lx = Parser $ \ts -> case ts of
  (t : _) -> Right (tokLexeme t == lx, ts)
  []      -> Right (False, ts)

-- | Consome o proximo token se ele for o lexema dado.
accept :: Lexeme -> Parser (Maybe Token)
accept lx = Parser $ \ts -> case ts of
  (t : rest) | tokLexeme t == lx -> Right (Just t, rest)
  _                              -> Right (Nothing, ts)

-- | Falha na posicao do proximo token.
failWith :: String -> String -> Parser a
failWith desc ctx = Parser $ \ts -> Left (unexpectedAt ts desc ctx)

-- | Falha numa posicao conhecida, independente do proximo token.
failAt :: Pos -> String -> String -> String -> Parser a
failAt p found desc ctx = Parser $ \_ -> Left (ParseError p found desc ctx)

unexpectedAt :: [Token] -> String -> String -> ParseError
unexpectedAt ts desc ctx = case ts of
  (t : _) -> ParseError (tokPos t) (showLexeme (tokLexeme t)) desc ctx
  []      -> ParseError initialPos "fim da entrada" desc ctx

-- | Zero ou mais repeticoes, enquanto a condicao for verdadeira.
many0 :: Parser Bool -> Parser a -> Parser [a]
many0 cond p = go
  where
    go = do
      continua <- cond
      if continua
        then (:) <$> p <*> go
        else pure []

-- | Um ou mais itens separados pelo lexema dado.
sepBy1 :: Lexeme -> Parser a -> Parser [a]
sepBy1 sep p = (:) <$> p <*> go
  where
    go = do
      m <- accept sep
      case m of
        Nothing -> pure []
        Just _  -> (:) <$> p <*> go

-- | Nome legivel de um lexema nas mensagens de erro, incluindo o texto
-- de literais e identificadores.
showLexeme :: Lexeme -> String
showLexeme lx = case lx of
  TIdent n     -> "identificador " ++ show n
  TIntLit n    -> "literal inteiro " ++ show n
  TFloatLit d  -> "literal float " ++ show d
  TStringLit s -> "literal string " ++ show s
  TBoolLit b   -> if b then "True" else "False"
  TNewline     -> "fim de linha"
  TIndent      -> "aumento de indentacao"
  TDedent      -> "fim de bloco"
  TEOF         -> "fim do arquivo"
  other        -> describeLexeme other

-- Reexportado de Syntax para evitar dependencia circular.
type Name = String
