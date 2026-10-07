-- | Camada de indentacao do analisador lexico.
--
-- Recebe a lista de tokens produzida pelo scanner e insere INDENT e
-- DEDENT conforme a secao 4.5 da especificacao. O nivel de indentacao
-- de uma linha e a coluna do seu primeiro token, informacao que o
-- scanner ja preservou.
module MiniPython.Frontend.Lexer.Indent
  ( layout
  ) where

import MiniPython.Frontend.Lexer.MiniPythonLexer (LexError (..))
import MiniPython.Frontend.Lexer.Token

-- | Insere INDENT e DEDENT na sequencia de tokens.
layout :: [Token] -> Either LexError [Token]
layout = startLine [0]

-- | Estado no inicio de uma linha logica: decide INDENT ou DEDENT
-- comparando a coluna do primeiro token com o topo da pilha.
startLine :: [Int] -> [Token] -> Either LexError [Token]
startLine stack toks = case toks of
  [] -> Right []
  (t : _)
    | tokLexeme t == TEOF ->
        -- Fim do arquivo: fecha todos os blocos abertos antes do EOF.
        Right (map (const (Token TDedent (tokPos t))) (closing stack) ++ toks)
  (t : _) ->
    let level = posCol (tokPos t) - 1
        top   = head stack
    in if level > top
         then (Token TIndent (tokPos t) :) <$> inLine (level : stack) toks
         else if level == top
                then inLine stack toks
                else case popTo level stack of
                       Nothing -> Left (LexError (tokPos t) "indentacao inconsistente")
                       Just (n, stack') ->
                         (replicate n (Token TDedent (tokPos t)) ++)
                           <$> inLine stack' toks

-- | Estado no meio de uma linha: copia tokens ate o NEWLINE e volta
-- para startLine.
inLine :: [Int] -> [Token] -> Either LexError [Token]
inLine stack toks = case toks of
  [] -> Right []
  (t : rest)
    | tokLexeme t == TNewline -> (t :) <$> startLine stack rest
    | otherwise               -> (t :) <$> inLine stack rest

-- | Niveis a fechar no fim do arquivo (todos menos o 0 inicial).
closing :: [Int] -> [Int]
closing = filter (/= 0)

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
