-- | Arvore sintatica abstrata da linguagem MiniPython.
--
-- Todo no carrega a posicao (linha e coluna) do token que o iniciou,
-- conforme exigido pelo enunciado. A posicao reutiliza o tipo Pos do
-- analisador lexico, de modo que ha uma unica nocao de posicao em todo
-- o compilador.
module MiniPython.Frontend.Syntax.MiniPythonSyntax
  ( Name
  , Program (..)
  , Decl (..)
  , FunDef (..)
  , Param (..)
  , ClassDef (..)
  , Field (..)
  , Block
  , Stmt (..)
  , LValue (..)
  , AugOp (..)
  , Expr (..)
  , BinOp (..)
  , UnOp (..)
  , Type (..)
  , exprPos
  , stmtPos
  , lvaluePos
  , typePos
  ) where

import MiniPython.Frontend.Lexer.Token (Pos)

-- | Identificadores do programa fonte.
type Name = String

-- | Um programa e uma sequencia de definicoes e comandos de nivel
-- superior, na ordem em que aparecem no arquivo. O Eq serve aos testes,
-- que comparam as ASTs dos tres parsers, posicoes incluidas.
newtype Program = Program [Decl]
  deriving (Eq)

-- | Item de nivel superior.
data Decl
  = DFun FunDef
  | DClass ClassDef
  | DStmt Stmt
  deriving (Eq)

-- | Definicao de funcao ou metodo. As anotacoes sao opcionais
-- (secao 6.2 da especificacao), por isso os campos Maybe.
data FunDef = FunDef
  { fnPos    :: Pos
  , fnName   :: Name
  , fnParams :: [Param]
  , fnReturn :: Maybe Type
  , fnBody   :: Block
  }
  deriving (Eq)

-- | Parametro formal de uma funcao.
data Param = Param
  { paramPos  :: Pos
  , paramName :: Name
  , paramType :: Maybe Type
  }
  deriving (Eq)

-- | Definicao de classe, com heranca simples opcional.
data ClassDef = ClassDef
  { clsPos     :: Pos
  , clsName    :: Name
  , clsSuper   :: Maybe Name
  , clsFields  :: [Field]
  , clsMethods :: [FunDef]
  }
  deriving (Eq)

-- | Campo de classe. A anotacao de tipo e obrigatoria (secao 6.3).
data Field = Field
  { fieldPos  :: Pos
  , fieldName :: Name
  , fieldType :: Type
  }
  deriving (Eq)

-- | Bloco: sequencia de comandos delimitada por INDENT e DEDENT.
type Block = [Stmt]

data Stmt
  -- | Declaracao com anotacao explicita: @nome: tipo = expr@.
  = SVarDecl Pos Name Type Expr
  -- | Atribuicao a variavel, elemento de lista ou campo de objeto.
  | SAssign Pos LValue Expr
  -- | Atribuicao aumentada: @alvo += expr@ e variantes.
  | SAugAssign Pos LValue AugOp Expr
  -- | Condicional. A lista guarda as clausulas elif na ordem; o ultimo
  -- bloco e o else, vazio quando ausente.
  | SIf Pos Expr Block [(Pos, Expr, Block)] Block
  | SWhile Pos Expr Block
  -- | @for nome in expr:@ sobre lista ou sobre range.
  | SFor Pos Name Expr Block
  -- | @return@ com ou sem expressao.
  | SReturn Pos (Maybe Expr)
  | SBreak Pos
  | SContinue Pos
  | SPass Pos
  -- | Expressao usada como comando, tipicamente uma chamada.
  | SExpr Pos Expr
  deriving (Eq)

-- | Alvo de uma atribuicao.
data LValue
  = LVar Pos Name
  | LIndex Pos Expr Expr
  | LField Pos Expr Name
  deriving (Eq)

-- | Operadores de atribuicao aumentada (secao 7.1).
data AugOp
  = AddAssign | SubAssign | MulAssign
  | DivAssign | FloorDivAssign | ModAssign
  deriving (Eq)

data Expr
  = EInt Pos Integer
  | EFloat Pos Double
  | EStr Pos String
  | EBool Pos Bool
  | ENone Pos
  | EVar Pos Name
  | ESelf Pos
  | EBinOp Pos BinOp Expr Expr
  | EUnOp Pos UnOp Expr
  -- | Chamada @f(e1, ..., en)@. O alvo e uma expressao para cobrir
  -- tambem @obj.metodo(...)@ e @Pai.__init__(...)@.
  | ECall Pos Expr [Expr]
  | EIndex Pos Expr Expr
  | EField Pos Expr Name
  | EList Pos [Expr]
  -- | @lambda x, y: corpo@. Parametros nao possuem anotacao (secao 6.11).
  | ELambda Pos [Name] Expr
  -- | Expressao condicional @e1 if cond else e2@ (secao 6.10).
  | ECond Pos Expr Expr Expr
  deriving (Eq)

data BinOp
  = Add | Sub | Mul | Div | FloorDiv | Mod | Pow
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  deriving (Eq)

data UnOp
  = Neg | Not
  deriving (Eq)

-- | Anotacoes de tipo (secao 3).
data Type
  = TyInt Pos
  | TyFloat Pos
  | TyBool Pos
  | TyStr Pos
  | TyNone Pos
  | TyList Pos Type
  -- | Tipo de funcao @(T1, ..., Tn) -> R@.
  | TyFun Pos [Type] Type
  -- | Tipo de classe: identificador iniciado por maiuscula.
  | TyClass Pos Name
  -- | Variavel de tipo: identificador minusculo nao primitivo (secao 3.5).
  | TyVar Pos Name
  deriving (Eq)

-- | Posicao de uma expressao, usada nas mensagens de erro.
exprPos :: Expr -> Pos
exprPos e = case e of
  EInt p _      -> p
  EFloat p _    -> p
  EStr p _      -> p
  EBool p _     -> p
  ENone p       -> p
  EVar p _      -> p
  ESelf p       -> p
  EBinOp p _ _ _  -> p
  EUnOp p _ _     -> p
  ECall p _ _   -> p
  EIndex p _ _  -> p
  EField p _ _  -> p
  EList p _     -> p
  ELambda p _ _ -> p
  ECond p _ _ _ -> p

stmtPos :: Stmt -> Pos
stmtPos s = case s of
  SVarDecl p _ _ _    -> p
  SAssign p _ _       -> p
  SAugAssign p _ _ _  -> p
  SIf p _ _ _ _       -> p
  SWhile p _ _        -> p
  SFor p _ _ _        -> p
  SReturn p _         -> p
  SBreak p            -> p
  SContinue p         -> p
  SPass p             -> p
  SExpr p _           -> p

lvaluePos :: LValue -> Pos
lvaluePos lv = case lv of
  LVar p _     -> p
  LIndex p _ _ -> p
  LField p _ _ -> p

typePos :: Type -> Pos
typePos t = case t of
  TyInt p     -> p
  TyFloat p   -> p
  TyBool p    -> p
  TyStr p     -> p
  TyNone p    -> p
  TyList p _  -> p
  TyFun p _ _ -> p
  TyClass p _ -> p
  TyVar p _   -> p
