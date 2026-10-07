-- | Analisador lexico ad-hoc da linguagem MiniPython.
--
-- Percorre o texto de entrada produzindo tokens com linha e coluna.
-- Este modulo NAO trata indentacao: ele emite um NEWLINE ao fim de cada
-- linha fisica e deixa a geracao de INDENT e DEDENT para a camada
-- MiniPython.Frontend.Lexer.Indent, que opera sobre esta saida.
module MiniPython.Frontend.Lexer.MiniPythonLexer
  ( LexError (..)
  , scan
  , formatLexError
  , renderToken
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (isPrefixOf)
import MiniPython.Frontend.Lexer.Token

-- | Erro lexico, sempre acompanhado da posicao onde foi detectado.
data LexError = LexError
  { errPos :: Pos
  , errMsg :: String
  }
  deriving (Eq)

-- | Mensagem de erro no formato arquivo:linha:coluna: descricao.
formatLexError :: FilePath -> LexError -> String
formatLexError file e =
  file
    ++ ":" ++ show (posLine (errPos e))
    ++ ":" ++ show (posCol (errPos e))
    ++ ": erro lexico: " ++ errMsg e

-- | Converte o texto de entrada em uma lista de tokens.
-- A lista sempre termina com TEOF quando o reconhecimento tem sucesso.
scan :: String -> Either LexError [Token]
scan = go initialPos
  where
    go :: Pos -> String -> Either LexError [Token]
    go p [] = Right [Token TEOF p]
    go p input@(c : cs)
      | c == '\n' = (Token TNewline p :) <$> go (nextLine p) cs
      | c == ' '  = go (bumpCol p 1) cs
      | c == '\r' = go (bumpCol p 1) cs
      | c == '\t' = go (tabStop p) cs
      | c == '#'  =
          let comment = takeWhile (/= '\n') cs
          in go (bumpCol p (1 + length comment)) (drop (length comment) cs)
      | isDigit c = fromResult p input (lexNumber input)
      | isAlpha c || c == '_' =
          let (lx, n) = lexIdent input in emit p input lx n
      | c == '"' || c == '\'' = fromResult p input (lexString c cs)
      | otherwise = case matchOperator input of
          Just (lx, n) -> emit p input lx n
          Nothing      -> Left (LexError p ("caractere invalido: " ++ show c))

    -- Converte o resultado de um reconhecedor local em erro posicionado
    -- ou em um token seguido do restante da varredura.
    fromResult :: Pos -> String -> Either String (Lexeme, Int) -> Either LexError [Token]
    fromResult p input r = case r of
      Left msg      -> Left (LexError p msg)
      Right (lx, n) -> emit p input lx n

    -- Emite um token de n caracteres e continua a partir do que sobrou.
    emit :: Pos -> String -> Lexeme -> Int -> Either LexError [Token]
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

-- | Identificadores e palavras reservadas.
lexIdent :: String -> (Lexeme, Int)
lexIdent input =
  let name = takeWhile isIdentChar input
  in (classify name, length name)
  where
    classify name = case lookup name keywords of
      Just lx -> lx
      Nothing -> TIdent name

isIdentChar :: Char -> Bool
isIdentChar ch = isAlphaNum ch || ch == '_'

-- | Literais numericos. Um ponto so inicia a parte fracionaria se for
-- seguido de digito, de modo que 1.foo nao e lido como float malformado.
lexNumber :: String -> Either String (Lexeme, Int)
lexNumber input =
  let (intPart, afterInt) = spanDigits input
      (fracPart, afterFrac) = case afterInt of
        ('.' : d : _) | isDigit d ->
          let (ds, r) = spanDigits (drop 1 afterInt) in ('.' : ds, r)
        _ -> ("", afterInt)
      (expPart, _) = case afterFrac of
        (e : r) | e == 'e' || e == 'E' -> readExponent e r afterFrac
        _ -> ("", afterFrac)
      text = intPart ++ fracPart ++ expPart
      clean = filter (/= '_') text
  in if null fracPart && null expPart
       then Right (TIntLit (read clean), length text)
       else Right (TFloatLit (read clean), length text)
  where
    readExponent e r fallback = case r of
      (s : r2) | s == '+' || s == '-' ->
        let (ds, r3) = spanDigits r2
        in if null ds then ("", fallback) else (e : s : ds, r3)
      _ ->
        let (ds, r2) = spanDigits r
        in if null ds then ("", fallback) else (e : ds, r2)

spanDigits :: String -> (String, String)
spanDigits = span (\ch -> isDigit ch || ch == '_')

-- | Literais de string. O tamanho consumido inclui as duas aspas e conta
-- cada sequencia de escape como dois caracteres do fonte.
lexString :: Char -> String -> Either String (Lexeme, Int)
lexString quote = consume 1 ""
  where
    consume _ _ [] = Left "string nao terminada antes do fim do arquivo"
    consume _ _ ('\n' : _) = Left "quebra de linha dentro de string"
    consume n acc (ch : rest)
      | ch == quote = Right (TStringLit (reverse acc), n + 1)
      | ch == '\\' = case rest of
          (e : rest2) -> case unescape e of
            Just val -> consume (n + 2) (val : acc) rest2
            Nothing  -> Left ("sequencia de escape invalida: \\" ++ [e])
          [] -> Left "string nao terminada antes do fim do arquivo"
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
  [ ("and", TAnd), ("break", TBreak), ("bool", TBool), ("class", TClass)
  , ("continue", TContinue), ("def", TDef), ("elif", TElif), ("else", TElse)
  , ("False", TBoolLit False), ("float", TFloat), ("for", TFor), ("if", TIf)
  , ("in", TIn), ("int", TInt), ("lambda", TLambda), ("None", TNone)
  , ("not", TNot), ("or", TOr), ("pass", TPass), ("return", TReturn)
  , ("self", TSelf), ("str", TStr), ("True", TBoolLit True), ("while", TWhile)
  ]

-- | Operadores e pontuacao, obrigatoriamente do mais longo para o mais
-- curto: // precisa ser testado antes de /, e //= antes de //.
operators :: [(String, Lexeme)]
operators =
  [ ("//=", TDoubleSlashAssign)
  , ("**", TDoubleStar), ("//", TDoubleSlash)
  , ("==", TEq), ("!=", TNeq), ("<=", TLe), (">=", TGe), ("->", TArrow)
  , ("+=", TPlusAssign), ("-=", TMinusAssign), ("*=", TStarAssign)
  , ("/=", TSlashAssign), ("%=", TPercentAssign)
  , ("+", TPlus), ("-", TMinus), ("*", TStar), ("/", TSlash)
  , ("%", TPercent), ("<", TLt), (">", TGt), ("=", TAssign)
  , ("(", TLParen), (")", TRParen), ("[", TLBracket), ("]", TRBracket)
  , (",", TComma), (":", TColon), (".", TDot)
  ]

matchOperator :: String -> Maybe (Lexeme, Int)
matchOperator input = foldr try Nothing operators
  where
    try (sym, lx) acc
      | sym `isPrefixOf` input = Just (lx, length sym)
      | otherwise              = acc

-- | Texto original aproximado de um lexema, para a saida de --lex.
lexemeText :: Lexeme -> String
lexemeText lx = case lx of
  TIntLit n    -> show n
  TFloatLit d  -> show d
  TStringLit s -> show s
  TBoolLit b   -> if b then "True" else "False"
  TIdent name  -> name
  TNewline     -> "\\n"
  TIndent      -> ""
  TDedent      -> ""
  TEOF         -> ""
  _ -> case [sym | (sym, l) <- keywords ++ operators, l == lx] of
         (sym : _) -> sym
         []        -> describeLexeme lx

-- | Uma linha da saida de --lex: tipo, lexema, linha e coluna.
renderToken :: Token -> String
renderToken t =
  pad 14 (describeLexeme (tokLexeme t))
    ++ pad 18 (lexemeText (tokLexeme t))
    ++ show (posLine (tokPos t)) ++ ":" ++ show (posCol (tokPos t))

pad :: Int -> String -> String
pad n s = s ++ replicate (max 1 (n - length s)) ' '
