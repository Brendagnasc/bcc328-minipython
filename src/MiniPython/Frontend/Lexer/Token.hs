-- | Tipos que representam os tokens da linguagem MiniPython.
module MiniPython.Frontend.Lexer.Token
  ( Pos (..)
  , Lexeme (..)
  , Token (..)
  , initialPos
  , describeLexeme
  , expectedLexeme
  ) where

-- | Posicao de um caractere no arquivo fonte.
-- Linhas e colunas sao contadas a partir de 1.
data Pos = Pos
  { posLine :: !Int
  , posCol  :: !Int
  }
  deriving (Eq, Ord)

-- | Posicao do primeiro caractere de um arquivo.
initialPos :: Pos
initialPos = Pos 1 1

-- | Classe de um token, sem informacao de posicao.
--
-- Os nomes dos construtores sao os da tabela da secao 2.2 do
-- enunciado, porque a saida de --lex e o proprio show do lexema.
-- O Ord nao tem significado semantico: e exigido pelo Megaparsec, que
-- guarda os tokens esperados numa estrutura ordenada.
data Lexeme
  = TkIntLit   Integer
  | TkFloatLit Double
  | TkStrLit   String
  | TkBoolLit  Bool
  | TkIdent    String
  | TkAnd | TkBreak | TkBoolKw | TkClass | TkContinue | TkDef | TkElif
  | TkElse | TkFloatKw | TkFor | TkIf | TkIn | TkIntKw | TkLambda
  | TkNoneKw | TkNot | TkOr | TkPass | TkReturn | TkSelf | TkStrKw | TkWhile
  | TkIndent | TkDedent | TkNewline
  | TkPlus | TkMinus | TkStar | TkSlash | TkDSlash | TkPercent | TkDStar
  | TkEqEq | TkNeq | TkLt | TkLe | TkGt | TkGe
  | TkAssign
  | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq | TkDSlashEq | TkPercentEq
  | TkLParen | TkRParen | TkLBracket | TkRBracket
  | TkComma | TkColon | TkDot | TkArrow
  | TkEOF
  deriving (Eq, Ord, Show)

-- | Um token e sua classe acompanhada da posicao onde ele comeca.
data Token = Token
  { tokLexeme :: Lexeme
  , tokPos    :: Pos
  }
  deriving (Eq, Ord)

-- | Texto do lexema como aparece no fonte. Para literais o texto e
-- reconstruido a partir do valor.
lexemeText :: Lexeme -> String
lexemeText l = case l of
  TkIntLit n   -> show n
  TkFloatLit d -> show d
  TkStrLit s   -> show s
  TkBoolLit b  -> if b then "True" else "False"
  TkIdent n    -> n
  TkAnd      -> "and"
  TkBreak    -> "break"
  TkBoolKw   -> "bool"
  TkClass    -> "class"
  TkContinue -> "continue"
  TkDef      -> "def"
  TkElif     -> "elif"
  TkElse     -> "else"
  TkFloatKw  -> "float"
  TkFor      -> "for"
  TkIf       -> "if"
  TkIn       -> "in"
  TkIntKw    -> "int"
  TkLambda   -> "lambda"
  TkNoneKw   -> "None"
  TkNot      -> "not"
  TkOr       -> "or"
  TkPass     -> "pass"
  TkReturn   -> "return"
  TkSelf     -> "self"
  TkStrKw    -> "str"
  TkWhile    -> "while"
  TkIndent  -> "INDENT"
  TkDedent  -> "DEDENT"
  TkNewline -> "NEWLINE"
  TkPlus    -> "+"
  TkMinus   -> "-"
  TkStar    -> "*"
  TkSlash   -> "/"
  TkDSlash  -> "//"
  TkPercent -> "%"
  TkDStar   -> "**"
  TkEqEq -> "=="
  TkNeq  -> "!="
  TkLt   -> "<"
  TkLe   -> "<="
  TkGt   -> ">"
  TkGe   -> ">="
  TkAssign    -> "="
  TkPlusEq    -> "+="
  TkMinusEq   -> "-="
  TkStarEq    -> "*="
  TkSlashEq   -> "/="
  TkDSlashEq  -> "//="
  TkPercentEq -> "%="
  TkLParen   -> "("
  TkRParen   -> ")"
  TkLBracket -> "["
  TkRBracket -> "]"
  TkComma    -> ","
  TkColon    -> ":"
  TkDot      -> "."
  TkArrow    -> "->"
  TkEOF -> "EOF"

-- | Como um token encontrado aparece nas mensagens de erro, no padrao
-- dos arquivos .err: tokens de layout pelo nome, os demais pelo texto
-- entre aspas simples.
describeLexeme :: Lexeme -> String
describeLexeme l = case l of
  TkIndent  -> "INDENT"
  TkDedent  -> "DEDENT"
  TkNewline -> "NEWLINE"
  TkEOF     -> "EOF"
  _         -> "'" ++ lexemeText l ++ "'"

-- | Como um token esperado aparece nas mensagens de erro.
expectedLexeme :: Lexeme -> String
expectedLexeme TkIndent = "INDENT (indented block)"
expectedLexeme l        = describeLexeme l
