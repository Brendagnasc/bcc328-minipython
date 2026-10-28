-- | Camada de indentacao do analisador lexico.
--
-- Recebe a lista de tokens produzida pelo scanner e insere INDENT e
-- DEDENT conforme a secao 4.5 da especificacao. O nivel de indentacao
-- de uma linha e a coluna do seu primeiro token, informacao que o
-- scanner ja preservou.
--
-- Posicoes, no formato da secao 2.2 do enunciado: o INDENT fica na
-- posicao do primeiro token da linha indentada; o DEDENT, e tambem o
-- erro de indentacao, ficam na posicao do NEWLINE que encerrou a
-- linha anterior, porque e ao fim dessa linha que o bloco se fecha.
--
-- Nao geram INDENT nem DEDENT (item 4 da secao 4.5):
--   * linhas em branco e linhas contendo apenas comentario, que chegam
--     aqui como um NEWLINE isolado porque o scanner descarta comentarios;
--   * linhas continuadas dentro de parenteses ou colchetes abertos.
module MiniPython.Frontend.Lexer.Indent
  ( layout
  ) where

import Data.Maybe (fromMaybe)
import MiniPython.Frontend.Lexer.Token
import MiniPython.Utils.Error

-- | Insere INDENT e DEDENT na sequencia de tokens.
layout :: [Token] -> Either Error [Token]
layout = startLine [0] Nothing

-- | Inicio de uma linha logica: compara a coluna do primeiro token com
-- o topo da pilha para decidir INDENT, DEDENT ou nada. O segundo
-- argumento e a posicao do NEWLINE da linha anterior.
startLine :: [Int] -> Maybe Pos -> [Token] -> Either Error [Token]
startLine stack lastNl toks = case toks of
  [] -> Right []

  -- Linha em branco ou so com comentario: nao afeta a pilha e o
  -- NEWLINE e descartado.
  (t : rest) | tokLexeme t == TkNewline -> startLine stack lastNl rest

  -- Fim do arquivo: fecha todos os blocos abertos antes do EOF.
  (t : _) | tokLexeme t == TkEOF ->
    Right (replicate (length stack - 1) (Token TkDedent (closeAt t)) ++ toks)

  (t : _) ->
    let level = posCol (tokPos t) - 1
        top   = head stack
    in case compare level top of
         GT -> (Token TkIndent (tokPos t) :) <$> inLine 0 (level : stack) toks
         EQ -> inLine 0 stack toks
         LT -> case popTo level stack of
                 Nothing -> Left (Error (closeAt t) (invalidLevel level))
                 Just (n, stack') ->
                   (replicate n (Token TkDedent (closeAt t)) ++)
                     <$> inLine 0 stack' toks
  where
    closeAt t = fromMaybe (tokPos t) lastNl

-- | Meio de uma linha. O primeiro argumento conta parenteses e colchetes
-- abertos: enquanto for maior que zero a linha logica continua, e as
-- quebras de linha sao descartadas.
inLine :: Int -> [Int] -> [Token] -> Either Error [Token]
inLine depth stack toks = case toks of
  [] -> Right []
  (t : rest) -> case tokLexeme t of
    TkNewline
      | depth > 0 -> inLine depth stack rest
      | otherwise -> (t :) <$> startLine stack (Just (tokPos t)) rest
    -- Ultima linha sem quebra de linha: o NEWLINE e emitido mesmo assim,
    -- como em Python, na posicao do fim do arquivo.
    TkEOF ->
      (Token TkNewline (tokPos t) :) <$> startLine stack (Just (tokPos t)) toks
    TkLParen   -> keep (depth + 1)
    TkLBracket -> keep (depth + 1)
    TkRParen   -> keep (max 0 (depth - 1))
    TkRBracket -> keep (max 0 (depth - 1))
    _          -> keep depth
    where
      keep d = (t :) <$> inLine d stack rest

-- | Mensagem do erro de indentacao, no texto dos arquivos .err.
invalidLevel :: Int -> String
invalidLevel level =
  "nível de indentação inválido: " ++ show level
    ++ " não está na pilha de indentação"

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
