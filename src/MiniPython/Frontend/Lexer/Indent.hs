-- | Camada de indentacao do analisador lexico.
--
-- Recebe a lista de tokens produzida pelo scanner e insere INDENT e
-- DEDENT conforme a secao 4.5 da especificacao. O nivel de indentacao
-- de uma linha e a coluna do seu primeiro token, informacao que o
-- scanner ja preservou.
--
-- Nao geram INDENT nem DEDENT (item 4 da secao 4.5):
--   * linhas em branco e linhas contendo apenas comentario, que chegam
--     aqui como um NEWLINE isolado porque o scanner descarta comentarios;
--   * linhas continuadas dentro de parenteses ou colchetes abertos.
module MiniPython.Frontend.Lexer.Indent
  ( layout
  ) where

import MiniPython.Frontend.Lexer.MiniPythonLexer (LexError (..))
import MiniPython.Frontend.Lexer.Token

-- | Insere INDENT e DEDENT na sequencia de tokens.
layout :: [Token] -> Either LexError [Token]
layout = startLine [0]

-- | Inicio de uma linha logica: compara a coluna do primeiro token com
-- o topo da pilha para decidir INDENT, DEDENT ou nada.
startLine :: [Int] -> [Token] -> Either LexError [Token]
startLine stack toks = case toks of
  [] -> Right []

  -- Fim do arquivo: fecha todos os blocos abertos antes do EOF.
  (t : _) | tokLexeme t == TEOF ->
    Right (replicate (length stack - 1) (Token TDedent (tokPos t)) ++ toks)

  -- Linha em branco ou so com comentario: nao afeta a pilha e o
  -- NEWLINE e descartado para nao poluir a saida.
  (t : rest) | tokLexeme t == TNewline -> startLine stack rest

  (t : _) ->
    let level = posCol (tokPos t) - 1
        top   = head stack
    in case compare level top of
         GT -> (Token TIndent (tokPos t) :) <$> inLine 0 (level : stack) toks
         EQ -> inLine 0 stack toks
         LT -> case popTo level stack of
                 Nothing -> Left (LexError (tokPos t) (inconsistente level stack))
                 Just (n, stack') ->
                   (replicate n (Token TDedent (tokPos t)) ++)
                     <$> inLine 0 stack' toks

-- | Meio de uma linha. O primeiro argumento conta parenteses e colchetes
-- abertos: enquanto for maior que zero a linha logica continua, e as
-- quebras de linha sao descartadas.
inLine :: Int -> [Int] -> [Token] -> Either LexError [Token]
inLine depth stack toks = case toks of
  [] -> Right []
  (t : rest) -> case tokLexeme t of
    TNewline
      | depth > 0 -> inLine depth stack rest
      | otherwise -> (t :) <$> startLine stack rest
    TLParen   -> keep (depth + 1)
    TLBracket -> keep (depth + 1)
    TRParen   -> keep (max 0 (depth - 1))
    TRBracket -> keep (max 0 (depth - 1))
    _         -> keep depth
    where
      keep d = (t :) <$> inLine d stack rest

-- | Mensagem de erro que mostra o nivel encontrado e os niveis validos.
inconsistente :: Int -> [Int] -> String
inconsistente level stack =
  "indentacao inconsistente: nivel " ++ show level
    ++ " nao corresponde a nenhum nivel aberto "
    ++ show (reverse stack)

-- | Desempilha ate encontrar o nivel dado, devolvendo quantos DEDENT
-- emitir e a pilha resultante. Nothing quando o nivel nao existe na
-- pilha, o que caracteriza indentacao inconsistente.
popTo :: Int -> [Int] -> Maybe (Int, [Int])
popTo level = go 0
  where
    go _ [] = Nothing
    go n st@(top : rest)
      | top == level = Just (n, st)
      | top >  level = go (n + 1) rest
      | otherwise    = Nothing
