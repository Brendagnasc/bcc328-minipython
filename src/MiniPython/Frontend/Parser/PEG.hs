{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | Analisador sintatico baseado em PEG, implementado com Megaparsec.
--
-- Reconhece exatamente a mesma linguagem da GLC proposta em
-- docs/gramatica.md e produz a mesma AST do analisador descendente
-- recursivo, de modo que as fases posteriores do compilador nao
-- dependem da estrategia de analise.
--
-- Diferenca essencial em relacao ao descendente recursivo: a escolha
-- em PEG e ordenada. Em @a <|> b@, b so e tentado se a falhar sem
-- consumir entrada; quando a alternativa pode consumir tokens antes
-- de falhar, e necessario envolve-la em try.
module MiniPython.Frontend.Parser.PEG
  ( parseProgramPEG
  , pegErrorPos
  , formatPegError
  ) where

import Control.Monad (void)
import Control.Monad.Combinators (many, sepBy, (<|>))
import Data.Char (isUpper)
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import qualified MiniPython.Frontend.Lexer.Token as L
import MiniPython.Frontend.Parser.Stream
import MiniPython.Frontend.Syntax.MiniPythonSyntax
import Text.Megaparsec hiding (Token, many, sepBy, (<|>))

-- | Reconhece um programa completo a partir da lista de tokens.
parseProgramPEG :: FilePath -> [L.Token] -> Either (ParseErrorBundle TokenStream Void) Program
parseProgramPEG file toks = runParser (pProgram <* eof') file (toStream toks)

-- | Posicao mais a direita em que o reconhecimento falhou, conforme
-- exigido na secao 4 do enunciado. O Megaparsec ja reporta o erro na
-- maior posicao alcancada entre todas as alternativas tentadas.
pegErrorPos :: ParseErrorBundle TokenStream Void -> L.Pos
pegErrorPos bundle = posFromSourcePos (pstateSourcePos estado)
  where
    erro = NE.head (bundleErrors bundle)
    (_, estado) = reachOffset (errorOffset erro) (bundlePosState bundle)

-- | Formata o erro do PEG no mesmo padrao dos demais analisadores.
-- O Megaparsec relata a falha na maior posicao alcancada entre todas
-- as alternativas tentadas, que e a posicao mais a direita exigida
-- pela secao 4 do enunciado.
formatPegError :: FilePath -> ParseErrorBundle TokenStream Void -> String
formatPegError file bundle =
  file
    ++ ":" ++ show (L.posLine p)
    ++ ":" ++ show (L.posCol p)
    ++ ": erro sintatico: " ++ detalhe
  where
    p = pegErrorPos bundle
    erro = NE.head (bundleErrors bundle)
    detalhe = unwords (lines (parseErrorTextPretty erro))

-- Consome o token EOF produzido pelo lexer e entao o fim do fluxo.
eof' :: PegParser ()
eof' = void (satisfyLex (== L.TkEOF)) >> eof

-- | Consome um token cujo lexema satisfaz o predicado.
satisfyLex :: (L.Lexeme -> Bool) -> PegParser L.Token
satisfyLex f = satisfy (f . L.tokLexeme)

-- | Consome um lexema especifico.
tok :: L.Lexeme -> PegParser L.Token
tok lx = satisfyLex (== lx)

-- | Consome um lexema descartando o resultado.
tok_ :: L.Lexeme -> PegParser ()
tok_ = void . tok

-- | Consome um identificador, devolvendo posicao e nome.
ident :: PegParser (L.Pos, Name)
ident = do
  t <- satisfyLex isIdent
  case L.tokLexeme t of
    L.TkIdent n -> pure (L.tokPos t, n)
    _          -> fail "identificador"
  where
    isIdent (L.TkIdent _) = True
    isIdent _            = False

-- Programa ::= { Decl } EOF
pProgram :: PegParser Program
pProgram = Program <$> many pDecl

-- Decl ::= DefFuncao / DefClasse / Comando
pDecl :: PegParser Decl
pDecl = choice
  [ DFun <$> pFunDef
  , DClass <$> pClassDef
  , DStmt <$> pStmt
  ]

-- DefFuncao ::= 'def' IDENT '(' [ Params ] ')' [ '->' Tipo ] ':' Bloco
pFunDef :: PegParser FunDef
pFunDef = do
  t <- tok L.TkDef
  (_, nome) <- ident
  ps <- between (tok_ L.TkLParen) (tok_ L.TkRParen) (pParam `sepBy` tok_ L.TkComma)
  ret <- optional (tok_ L.TkArrow *> pType)
  tok_ L.TkColon
  FunDef (L.tokPos t) nome ps ret <$> pBlock

pParam :: PegParser Param
pParam = pSelfParam <|> pNamedParam
  where
    pSelfParam = do
      t <- tok L.TkSelf
      pure (Param (L.tokPos t) "self" Nothing)
    pNamedParam = do
      (p, nome) <- ident
      anot <- optional (tok_ L.TkColon *> pType)
      pure (Param p nome anot)

-- DefClasse ::= 'class' IDENT [ '(' IDENT ')' ] ':' BlocoClasse
pClassDef :: PegParser ClassDef
pClassDef = do
  t <- tok L.TkClass
  (_, nome) <- ident
  super <- optional (between (tok_ L.TkLParen) (tok_ L.TkRParen) (snd <$> ident))
  tok_ L.TkColon
  tok_ L.TkNewline
  tok_ L.TkIndent
  campos <- many (try pField)
  metodos <- many pFunDef
  _ <- optional (tok_ L.TkPass *> tok_ L.TkNewline)
  tok_ L.TkDedent
  pure (ClassDef (L.tokPos t) nome super campos metodos)

-- Campo ::= IDENT ':' Tipo NEWLINE
--
-- Envolvido em try no ponto de uso: um campo e um metodo nao se
-- distinguem pelo primeiro token quando o corpo da classe comeca
-- por um comando.
pField :: PegParser Field
pField = do
  (p, nome) <- ident
  tok_ L.TkColon
  t <- pType
  tok_ L.TkNewline
  pure (Field p nome t)

-- Bloco ::= NEWLINE INDENT Comando { Comando } DEDENT
pBlock :: PegParser Block
pBlock = do
  tok_ L.TkNewline
  between (tok_ L.TkIndent) (tok_ L.TkDedent) (some' pStmt)
  where
    some' p = (:) <$> p <*> many p

-- Comando ::= ComandoSimples NEWLINE / ComandoComposto
pStmt :: PegParser Stmt
pStmt = choice [pIf, pWhile, pFor, pSimpleStmt]

pIf :: PegParser Stmt
pIf = do
  t <- tok L.TkIf
  cond <- pExpr
  tok_ L.TkColon
  entao <- pBlock
  elifs <- many pElif
  senao <- option [] (tok_ L.TkElse *> tok_ L.TkColon *> pBlock)
  pure (SIf (L.tokPos t) cond entao elifs senao)

pElif :: PegParser (L.Pos, Expr, Block)
pElif = do
  t <- tok L.TkElif
  cond <- pExpr
  tok_ L.TkColon
  corpo <- pBlock
  pure (L.tokPos t, cond, corpo)

pWhile :: PegParser Stmt
pWhile = do
  t <- tok L.TkWhile
  cond <- pExpr
  tok_ L.TkColon
  SWhile (L.tokPos t) cond <$> pBlock

pFor :: PegParser Stmt
pFor = do
  t <- tok L.TkFor
  (_, v) <- ident
  tok_ L.TkIn
  it <- pExpr
  tok_ L.TkColon
  SFor (L.tokPos t) v it <$> pBlock

pSimpleStmt :: PegParser Stmt
pSimpleStmt = pSimple <* tok_ L.TkNewline

pSimple :: PegParser Stmt
pSimple = choice
  [ SPass . L.tokPos <$> tok L.TkPass
  , SBreak . L.tokPos <$> tok L.TkBreak
  , SContinue . L.tokPos <$> tok L.TkContinue
  , pReturn
  , try pVarDecl
  , pAssignOrExpr
  ]

pReturn :: PegParser Stmt
pReturn = do
  t <- tok L.TkReturn
  e <- optional pExpr
  pure (SReturn (L.tokPos t) e)

-- Atribuicao ::= IDENT ':' Tipo '=' Expr
--
-- Precisa de try: compartilha o prefixo IDENT com a atribuicao comum
-- e so se distingue no segundo token.
pVarDecl :: PegParser Stmt
pVarDecl = do
  (p, nome) <- ident
  tok_ L.TkColon
  t <- pType
  tok_ L.TkAssign
  SVarDecl p nome t <$> pExpr

-- Reconhece uma expressao e, se vier em seguida um operador de
-- atribuicao, converte a expressao ja construida em alvo. Mesma
-- estrategia do analisador descendente recursivo.
pAssignOrExpr :: PegParser Stmt
pAssignOrExpr = do
  e <- pExpr
  mop <- optional (choice (map opParser augOps) <|> (Nothing <$ tok L.TkAssign))
  case mop of
    Nothing -> pure (SExpr (exprPos e) e)
    Just Nothing -> do
      alvo <- toLValue e
      SAssign (exprPos e) alvo <$> pExpr
    Just (Just op) -> do
      alvo <- toLValue e
      SAugAssign (exprPos e) alvo op <$> pExpr
  where
    opParser (lx, op) = Just op <$ tok lx

toLValue :: Expr -> PegParser LValue
toLValue e = case e of
  EVar p n     -> pure (LVar p n)
  EIndex p b i -> pure (LIndex p b i)
  EField p b n -> pure (LField p b n)
  _            -> fail "alvo de atribuicao valido"

augOps :: [(L.Lexeme, AugOp)]
augOps =
  [ (L.TkPlusEq, AddAssign), (L.TkMinusEq, SubAssign)
  , (L.TkStarEq, MulAssign), (L.TkSlashEq, DivAssign)
  , (L.TkDSlashEq, FloorDivAssign), (L.TkPercentEq, ModAssign)
  ]

-- Tipos
pType :: PegParser Type
pType = choice
  [ TyInt . L.tokPos <$> tok L.TkIntKw
  , TyFloat . L.tokPos <$> tok L.TkFloatKw
  , TyBool . L.tokPos <$> tok L.TkBoolKw
  , TyStr . L.tokPos <$> tok L.TkStrKw
  , TyNone . L.tokPos <$> tok L.TkNoneKw
  , pTyFun
  , pTyListOrName
  ]

pTyFun :: PegParser Type
pTyFun = do
  t <- tok L.TkLParen
  args <- pType `sepBy` tok_ L.TkComma
  tok_ L.TkRParen
  tok_ L.TkArrow
  TyFun (L.tokPos t) args <$> pType

pTyListOrName :: PegParser Type
pTyListOrName = do
  (p, n) <- ident
  if n == "list"
    then TyList p <$> between (tok_ L.TkLBracket) (tok_ L.TkRBracket) pType
    else pure (if not (null n) && isUpper (head n) then TyClass p n else TyVar p n)

-- Expressoes, estratificadas como na GLC.
pExpr :: PegParser Expr
pExpr = do
  e <- pOr
  mc <- optional (tok_ L.TkIf *> ((,) <$> pOr <*> (tok_ L.TkElse *> pExpr)))
  pure $ case mc of
    Nothing          -> e
    Just (cond, alt) -> ECond (exprPos e) e cond alt

pOr :: PegParser Expr
pOr = chainl1 pAnd [(L.TkOr, Or)]

pAnd :: PegParser Expr
pAnd = chainl1 pNot [(L.TkAnd, And)]

pNot :: PegParser Expr
pNot = (do t <- tok L.TkNot; EUnOp (L.tokPos t) Not <$> pNot) <|> pRel

pRel :: PegParser Expr
pRel = do
  left <- pAdd
  mop <- optional (choice [ op <$ tok lx | (lx, op) <- relOps ])
  case mop of
    Nothing -> pure left
    Just op -> EBinOp (exprPos left) op left <$> pAdd

pAdd :: PegParser Expr
pAdd = chainl1 pMul [(L.TkPlus, Add), (L.TkMinus, Sub)]

pMul :: PegParser Expr
pMul = chainl1 pUnary mulOps

pUnary :: PegParser Expr
pUnary = (do t <- tok L.TkMinus; EUnOp (L.tokPos t) Neg <$> pUnary) <|> pPow

pPow :: PegParser Expr
pPow = do
  base <- pPostfix
  mp <- optional (tok_ L.TkDStar *> pPow)
  pure $ case mp of
    Nothing -> base
    Just e  -> EBinOp (exprPos base) Pow base e

pPostfix :: PegParser Expr
pPostfix = pAtom >>= go
  where
    go acc = choice
      [ do args <- between (tok_ L.TkLParen) (tok_ L.TkRParen) (pExpr `sepBy` tok_ L.TkComma)
           go (ECall (exprPos acc) acc args)
      , do idx <- between (tok_ L.TkLBracket) (tok_ L.TkRBracket) pExpr
           go (EIndex (exprPos acc) acc idx)
      , do tok_ L.TkDot
           (_, n) <- ident
           go (EField (exprPos acc) acc n)
      , pure acc
      ]

pAtom :: PegParser Expr
pAtom = do
  t <- lookAhead anySingle
  let p = L.tokPos t
  case L.tokLexeme t of
    L.TkIntLit n    -> anySingle >> pure (EInt p n)
    L.TkFloatLit d  -> anySingle >> pure (EFloat p d)
    L.TkStrLit s -> anySingle >> pure (EStr p s)
    L.TkBoolLit b   -> anySingle >> pure (EBool p b)
    L.TkNoneKw        -> anySingle >> pure (ENone p)
    L.TkSelf        -> anySingle >> pure (ESelf p)
    L.TkIdent n     -> anySingle >> pure (EVar p n)
    L.TkIntKw         -> anySingle >> pure (EVar p "int")
    L.TkFloatKw       -> anySingle >> pure (EVar p "float")
    L.TkStrKw         -> anySingle >> pure (EVar p "str")
    L.TkBoolKw        -> anySingle >> pure (EVar p "bool")
    L.TkLBracket    -> do
      _ <- anySingle
      es <- pExpr `sepBy` tok_ L.TkComma
      tok_ L.TkRBracket
      pure (EList p es)
    L.TkLambda -> do
      _ <- anySingle
      ps <- (snd <$> ident) `sepBy` tok_ L.TkComma
      tok_ L.TkColon
      ELambda p ps <$> pExpr
    L.TkLParen -> do
      _ <- anySingle
      e <- pExpr
      tok_ L.TkRParen
      pure e
    _ -> fail "expressao"

-- Cadeia associativa a esquerda.
chainl1 :: PegParser Expr -> [(L.Lexeme, BinOp)] -> PegParser Expr
chainl1 next ops = next >>= go
  where
    go acc = (do
      op <- choice [ o <$ tok lx | (lx, o) <- ops ]
      right <- next
      go (EBinOp (exprPos acc) op acc right)) <|> pure acc

relOps :: [(L.Lexeme, BinOp)]
relOps =
  [ (L.TkEqEq, Eq), (L.TkNeq, Neq), (L.TkLt, Lt)
  , (L.TkLe, Le), (L.TkGt, Gt), (L.TkGe, Ge)
  ]

mulOps :: [(L.Lexeme, BinOp)]
mulOps =
  [ (L.TkStar, Mul), (L.TkSlash, Div)
  , (L.TkDSlash, FloorDiv), (L.TkPercent, Mod)
  ]
