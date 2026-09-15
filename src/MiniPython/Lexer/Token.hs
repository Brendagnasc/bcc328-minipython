-- | Tipos que representam os tokens da linguagem MiniPython.
module MiniPython.Lexer.Token
  ( Pos (..)
  , Lexeme (..)
  , Token (..)
  , initialPos
  , describeLexeme
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
data Lexeme
  = TIntLit    Integer
  | TFloatLit  Double
  | TStringLit String
  | TBoolLit   Bool
  | TIdent String
  | TAnd | TBreak | TBool | TClass | TContinue | TDef | TElif | TElse
  | TFalse | TFloat | TFor | TIf | TIn | TInt | TLambda | TNone
  | TNot | TOr | TPass | TReturn | TSelf | TStr | TTrue | TWhile
  | TPlus | TMinus | TStar | TSlash | TDoubleSlash | TPercent | TDoubleStar
  | TEq | TNeq | TLt | TLe | TGt | TGe
  | TAssign
  | TPlusAssign | TMinusAssign | TStarAssign
  | TSlashAssign | TDoubleSlashAssign | TPercentAssign
  | TLParen | TRParen | TLBracket | TRBracket
  | TComma | TColon | TDot | TArrow
  | TNewline | TIndent | TDedent | TEOF
  deriving (Eq)

-- | Um token e sua classe acompanhada da posicao onde ele comeca.
data Token = Token
  { tokLexeme :: Lexeme
  , tokPos    :: Pos
  }
  deriving (Eq)

-- | Nome legivel da classe de um token, usado nas mensagens de erro
-- e na saida da opcao --lex.
describeLexeme :: Lexeme -> String
describeLexeme l = case l of
  TIntLit _    -> "INT_LIT"
  TFloatLit _  -> "FLOAT_LIT"
  TStringLit _ -> "STRING_LIT"
  TBoolLit _   -> "BOOL_LIT"
  TIdent _     -> "IDENT"
  TAnd      -> "KW_AND"
  TBreak    -> "KW_BREAK"
  TBool     -> "KW_BOOL"
  TClass    -> "KW_CLASS"
  TContinue -> "KW_CONTINUE"
  TDef      -> "KW_DEF"
  TElif     -> "KW_ELIF"
  TElse     -> "KW_ELSE"
  TFalse    -> "KW_FALSE"
  TFloat    -> "KW_FLOAT"
  TFor      -> "KW_FOR"
  TIf       -> "KW_IF"
  TIn       -> "KW_IN"
  TInt      -> "KW_INT"
  TLambda   -> "KW_LAMBDA"
  TNone     -> "KW_NONE"
  TNot      -> "KW_NOT"
  TOr       -> "KW_OR"
  TPass     -> "KW_PASS"
  TReturn   -> "KW_RETURN"
  TSelf     -> "KW_SELF"
  TStr      -> "KW_STR"
  TTrue     -> "KW_TRUE"
  TWhile    -> "KW_WHILE"
  TPlus        -> "PLUS"
  TMinus       -> "MINUS"
  TStar        -> "STAR"
  TSlash       -> "SLASH"
  TDoubleSlash -> "DSLASH"
  TPercent     -> "PERCENT"
  TDoubleStar  -> "DSTAR"
  TEq  -> "EQ"
  TNeq -> "NEQ"
  TLt  -> "LT"
  TLe  -> "LE"
  TGt  -> "GT"
  TGe  -> "GE"
  TAssign            -> "ASSIGN"
  TPlusAssign        -> "PLUS_ASSIGN"
  TMinusAssign       -> "MINUS_ASSIGN"
  TStarAssign        -> "STAR_ASSIGN"
  TSlashAssign       -> "SLASH_ASSIGN"
  TDoubleSlashAssign -> "DSLASH_ASSIGN"
  TPercentAssign     -> "PERCENT_ASSIGN"
  TLParen   -> "LPAREN"
  TRParen   -> "RPAREN"
  TLBracket -> "LBRACKET"
  TRBracket -> "RBRACKET"
  TComma    -> "COMMA"
  TColon    -> "COLON"
  TDot      -> "DOT"
  TArrow    -> "ARROW"
  TNewline -> "NEWLINE"
  TIndent  -> "INDENT"
  TDedent  -> "DEDENT"
  TEOF     -> "EOF"
