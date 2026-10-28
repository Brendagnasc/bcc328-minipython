-- | Analisador lexico ad-hoc da linguagem MiniPython.
--
-- Percorre o texto de entrada produzindo tokens com linha e coluna.
-- Este modulo NAO trata indentacao: ele emite um NEWLINE ao fim de cada
-- linha fisica e deixa a geracao de INDENT e DEDENT para a camada
-- MiniPython.Frontend.Lexer.Indent, que opera sobre esta saida.
module MiniPython.Frontend.Lexer.MiniPythonLexer
  ( scan
  , renderToken
  ) where

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)
import Data.List (isPrefixOf)
import MiniPython.Frontend.Lexer.Token
import MiniPython.Utils.Error

-- | Converte o texto de entrada em uma lista de tokens.
-- A lista sempre termina com TkEOF quando o reconhecimento tem sucesso.
scan :: String -> Either Error [Token]
scan = go initialPos
  where
    go :: Pos -> String -> Either Error [Token]
    go p [] = Right [Token TkEOF p]
    go p input@(c : cs)
      | c == '\n' = (Token TkNewline p :) <$> go (nextLine p) cs
      | c == ' '  = go (bumpCol p 1) cs
      | c == '\r' = go (bumpCol p 1) cs
      | c == '\t' = go (tabStop p) cs
      | c == '#'  =
          let comment = takeWhile (/= '\n') cs
          in go (bumpCol p (1 + length comment)) (drop (length comment) cs)
      | isDigit c = fromResult p input (lexNumber input)
      | isIdentStart c =
          let (lx, n) = lexIdent input in emit p input lx n
      | c == '"' || c == '\'' = fromResult p input (lexString c cs)
      | otherwise = case matchOperator input of
          Just (lx, n) -> emit p input lx n
          Nothing      -> Left (Error p ("caractere inválido: '" ++ [c] ++ "'"))

    -- Converte o resultado de um reconhecedor local em erro posicionado
    -- ou em um token seguido do restante da varredura.
    fromResult :: Pos -> String -> Either String (Lexeme, Int) -> Either Error [Token]
    fromResult p input r = case r of
      Left msg      -> Left (Error p msg)
      Right (lx, n) -> emit p input lx n

    -- Emite um token de n caracteres e continua a partir do que sobrou.
    emit :: Pos -> String -> Lexeme -> Int -> Either Error [Token]
    emit p input lx n = (Token lx p :) <$> go (bumpCol p n) (drop n input)

-- | Inicio da proxima linha.
nextLine :: Pos -> Pos
nextLine p = Pos (posLine p + 1) 1

-- | Avanca n colunas na mesma linha.
bumpCol :: Pos -> Int -> Pos
bumpCol p n = p { posCol = posCol p + n }

-- | Tabulacao avanca ate a proxima coluna multipla de 8,
-- conforme a secao 4.5 da especificacao.
tabStop :: Pos -> Pos
tabStop p = p { posCol = ((posCol p - 1) `div` 8 + 1) * 8 + 1 }

-- | Identificadores sao ASCII (secao 4.4); uma letra acentuada fora de
-- string ou comentario e caractere invalido.
isIdentStart :: Char -> Bool
isIdentStart ch = isAscii ch && (isAlpha ch || ch == '_')

isIdentChar :: Char -> Bool
isIdentChar ch = isAscii ch && (isAlphaNum ch || ch == '_')

-- | Identificadores e palavras reservadas.
lexIdent :: String -> (Lexeme, Int)
lexIdent input =
  let name = takeWhile isIdentChar input
  in (classify name, length name)
  where
    classify name = case lookup name keywords of
      Just lx -> lx
      Nothing -> TkIdent name

-- | Literais numericos. Depois do ponto decimal precisa vir ao menos um
-- digito (3. e mal formado), o expoente precisa de digitos (1e e 1e+
-- sao mal formados) e o separador _ so vale entre dois digitos (1__2 e
-- 1_ sao mal formados). O erro e apontado no inicio do numero.
lexNumber :: String -> Either String (Lexeme, Int)
lexNumber input = do
  let (intPart, afterInt) = spanDigits input
  (fracPart, afterFrac) <- case afterInt of
    ('.' : d : _) | isDigit d ->
      let (ds, r) = spanDigits (drop 1 afterInt) in Right ('.' : ds, r)
    ('.' : _) -> malFormado "float" (intPart ++ ".")
    _ -> Right ("", afterInt)
  expPart <- case afterFrac of
    (e : r) | e == 'e' || e == 'E' -> readExponent (intPart ++ fracPart) e r
    _ -> Right ""
  let text = intPart ++ fracPart ++ expPart
      clean = filter (/= '_') text
      isFloat = not (null fracPart && null expPart)
  if not (validSeparators text)
    then malFormado (if isFloat then "float" else "inteiro") text
    else if isFloat
      then Right (TkFloatLit (read clean), length text)
      else Right (TkIntLit (read clean), length text)
  where
    readExponent before e r = case r of
      (s : r2) | s == '+' || s == '-' -> digitsAfter [e, s] r2
      _                               -> digitsAfter [e] r
      where
        digitsAfter prefix rest = case spanDigits rest of
          ("", _) -> malFormado "float" (before ++ prefix)
          (ds, _) -> Right (prefix ++ ds)

    malFormado tipo text =
      Left ("literal " ++ tipo ++ " mal formado: '" ++ text ++ "'")

spanDigits :: String -> (String, String)
spanDigits = span (\ch -> isDigit ch || ch == '_')

-- | O separador _ so vale entre dois digitos.
validSeparators :: String -> Bool
validSeparators s =
  and [isDigit a && isDigit b | (a, '_', b) <- zip3 (' ' : s) s (drop 1 s ++ " ")]

-- | Literais de string. O tamanho consumido inclui as duas aspas e conta
-- cada sequencia de escape como dois caracteres do fonte.
lexString :: Char -> String -> Either String (Lexeme, Int)
lexString quote = consume 1 ""
  where
    consume _ _ [] = Left "string não terminada"
    consume _ _ ('\n' : _) = Left "string não terminada"
    consume n acc (ch : rest)
      | ch == quote = Right (TkStrLit (reverse acc), n + 1)
      | ch == '\\' = case rest of
          (e : rest2) | e /= '\n' -> case unescape e of
            Just val -> consume (n + 2) (val : acc) rest2
            Nothing  -> Left ("sequência de escape inválida: '\\" ++ [e] ++ "'")
          _ -> Left "string não terminada"
      | otherwise = consume (n + 1) (ch : acc) rest

unescape :: Char -> Maybe Char
unescape 'n'  = Just '\n'
unescape 't'  = Just '\t'
unescape 'r'  = Just '\r'
unescape '\\' = Just '\\'
unescape '"'  = Just '"'
unescape '\'' = Just '\''
unescape _    = Nothing

-- | Palavras reservadas. True e False viram literais booleanos.
keywords :: [(String, Lexeme)]
keywords =
  [ ("and", TkAnd), ("break", TkBreak), ("bool", TkBoolKw), ("class", TkClass)
  , ("continue", TkContinue), ("def", TkDef), ("elif", TkElif), ("else", TkElse)
  , ("False", TkBoolLit False), ("float", TkFloatKw), ("for", TkFor), ("if", TkIf)
  , ("in", TkIn), ("int", TkIntKw), ("lambda", TkLambda), ("None", TkNoneKw)
  , ("not", TkNot), ("or", TkOr), ("pass", TkPass), ("return", TkReturn)
  , ("self", TkSelf), ("str", TkStrKw), ("True", TkBoolLit True), ("while", TkWhile)
  ]

-- | Operadores e pontuacao, obrigatoriamente do mais longo para o mais
-- curto: // precisa ser testado antes de /, e //= antes de //.
operators :: [(String, Lexeme)]
operators =
  [ ("//=", TkDSlashEq)
  , ("**", TkDStar), ("//", TkDSlash)
  , ("==", TkEqEq), ("!=", TkNeq), ("<=", TkLe), (">=", TkGe), ("->", TkArrow)
  , ("+=", TkPlusEq), ("-=", TkMinusEq), ("*=", TkStarEq)
  , ("/=", TkSlashEq), ("%=", TkPercentEq)
  , ("+", TkPlus), ("-", TkMinus), ("*", TkStar), ("/", TkSlash)
  , ("%", TkPercent), ("<", TkLt), (">", TkGt), ("=", TkAssign)
  , ("(", TkLParen), (")", TkRParen), ("[", TkLBracket), ("]", TkRBracket)
  , (",", TkComma), (":", TkColon), (".", TkDot)
  ]

matchOperator :: String -> Maybe (Lexeme, Int)
matchOperator input = foldr try Nothing operators
  where
    try (sym, lx) acc
      | sym `isPrefixOf` input = Just (lx, length sym)
      | otherwise              = acc

-- | Uma linha da saida de --lex (secao 2.2 do enunciado):
-- @linha:coluna Construtor@. O EOF sai sempre com posicao 0:0.
renderToken :: Token -> String
renderToken (Token TkEOF _) = "0:0 TkEOF"
renderToken t =
  show (posLine p) ++ ":" ++ show (posCol p) ++ " " ++ show (tokLexeme t)
  where
    p = tokPos t
