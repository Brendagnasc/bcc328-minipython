-- | Analisador sintatico descendente recursivo, escrito com Megaparsec.
--
-- Cada funcao corresponde a um nao terminal da GLC proposta em
-- docs/gramatica.md. O parser le a lista de tokens do analisador
-- lexico, ja com INDENT e DEDENT, e escolhe cada producao olhando o
-- proximo token; so a declaracao de variavel e o campo de classe
-- precisam ver dois (IDENT seguido de ':'). Como nenhuma alternativa e
-- tentada e desfeita, nao ha try.
--
-- As mensagens de erro sao montadas aqui mesmo, no formato dos arquivos
-- .err (expected X, got Y), e viajam como erro customizado do
-- Megaparsec, que as devolve sem alteracao.
module MiniPython.Frontend.Parser.RecursiveDescent
  ( parseProgram
  ) where

import Data.Char (isUpper)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import MiniPython.Frontend.Lexer.Token
import MiniPython.Frontend.Parser.Stream
import MiniPython.Frontend.Syntax.MiniPythonSyntax
import MiniPython.Frontend.Syntax.Validate (duplicateParam)
import MiniPython.Utils.Error
import Text.Megaparsec
  ( ErrorFancy (..)
  , ParseError (..)
  , ParseErrorBundle (..)
  , Parsec
  , ShowErrorComponent (..)
  , anySingle
  , customFailure
  , errorOffset
  , getInput
  , lookAhead
  , runParser
  )

-- | Erro customizado: a mensagem pronta e a posicao a que ela se refere.
newtype Failure = Failure Error
  deriving (Eq, Ord)

instance ShowErrorComponent Failure where
  showErrorComponent (Failure e) = errMsg e

type Parser = Parsec Failure TokenStream

-- | Reconhece um programa completo a partir da lista de tokens.
parseProgram :: [Token] -> Either Error Program
parseProgram toks = case runParser pProgram "" (toStream toks) of
  Right prog  -> Right prog
  Left bundle -> Left (bundleError toks bundle)

-- | Todas as falhas deste parser sao customizadas. O caso trivial so
-- existe para que a funcao seja total.
bundleError :: [Token] -> ParseErrorBundle TokenStream Failure -> Error
bundleError toks bundle = case NE.head (bundleErrors bundle) of
  FancyError _ items | (ErrorCustom (Failure e) : _) <- Set.toList items -> e
  err -> case drop (errorOffset err) toks of
    (t : _) -> Error (tokPos t) ("unexpected " ++ describeLexeme (tokLexeme t))
    []      -> Error initialPos "unexpected end of input"

-- Primitivas ----------------------------------------------------------

-- | Proximo token, sem consumir. O fluxo sempre termina em TkEOF, que
-- so e consumido no fim de pProgram.
peekToken :: Parser Token
peekToken = lookAhead anySingle

peekLexeme :: Parser Lexeme
peekLexeme = tokLexeme <$> peekToken

-- | Os dois proximos lexemas.
peekTwo :: Parser (Lexeme, Lexeme)
peekTwo = do
  TokenStream ts <- getInput
  pure $ case map tokLexeme ts of
    (a : b : _) -> (a, b)
    [a]         -> (a, TkEOF)
    []          -> (TkEOF, TkEOF)

advance :: Parser Token
advance = anySingle

failAt :: Pos -> String -> Parser a
failAt p msg = customFailure (Failure (Error p msg))

-- | Falha com "expected X, got Y", sendo Y o proximo token.
expected :: String -> Parser a
expected what = do
  t <- peekToken
  failAt (tokPos t) ("expected " ++ what ++ ", got " ++ describeLexeme (tokLexeme t))

-- | Consome o lexema dado ou falha dizendo que era ele o esperado.
expect :: Lexeme -> Parser Token
expect lx = do
  l <- peekLexeme
  if l == lx then advance else expected (expectedLexeme lx)

-- | Consome o lexema se ele for o proximo.
accept :: Lexeme -> Parser Bool
accept lx = do
  l <- peekLexeme
  if l == lx then True <$ advance else pure False

expectIdent :: Parser (Pos, Name)
expectIdent = do
  t <- peekToken
  case tokLexeme t of
    TkIdent n -> (tokPos t, n) <$ advance
    _         -> expected "identifier"

-- | Um ou mais itens separados por virgula.
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = (:) <$> p <*> go
  where
    go = do
      more <- accept TkComma
      if more then (:) <$> p <*> go else pure []

-- | Lista separada por virgula, possivelmente vazia, que termina no
-- lexema dado. O lexema final nao e consumido.
commaSepUntil :: Lexeme -> Parser a -> Parser [a]
commaSepUntil close p = do
  l <- peekLexeme
  if l == close then pure [] else commaSep1 p

-- Programa e declaracoes ----------------------------------------------

-- | Programa ::= { Decl } EOF
pProgram :: Parser Program
pProgram = Program <$> go
  where
    go = do
      l <- peekLexeme
      if l == TkEOF then [] <$ advance else (:) <$> pDecl <*> go

-- | Decl ::= DefClasse | Comando
--
-- Uma DefFuncao de nivel superior, que a GLC deriva por Comando, vira
-- DFun na AST; dentro de um bloco ela vira o comando SFunDef.
pDecl :: Parser Decl
pDecl = do
  l <- peekLexeme
  case l of
    TkDef   -> DFun <$> pFunDef
    TkClass -> DClass <$> pClassDef
    _       -> DStmt <$> pStmt

-- | DefFuncao ::= 'def' IDENT '(' [ Params ] ')' [ '->' Tipo ] ':' Bloco
pFunDef :: Parser FunDef
pFunDef = do
  t <- expect TkDef
  (_, nome) <- expectIdent
  _ <- expect TkLParen
  ps <- commaSepUntil TkRParen pParam
  fecha <- expect TkRParen
  -- O parametro repetido e apontado no ')', quando a lista ja foi lida.
  mapM_ (\n -> failAt (tokPos fecha) ("duplicate parameter: '" ++ n ++ "'"))
        (duplicateParam ps)
  ret <- pOptReturn
  _ <- expect TkColon
  FunDef (tokPos t) nome ps ret <$> pBlock

pOptReturn :: Parser (Maybe Type)
pOptReturn = do
  seta <- accept TkArrow
  if seta then Just <$> pType else pure Nothing

-- | Param ::= 'self' | IDENT [ ':' Tipo ]
--
-- Em metodos, self aparece como primeiro parametro. Como self e
-- palavra reservada (secao 4.2), ele nao casa com IDENT e e tratado
-- a parte.
pParam :: Parser Param
pParam = do
  t <- peekToken
  case tokLexeme t of
    TkSelf -> Param (tokPos t) "self" Nothing <$ advance
    _ -> do
      (p, nome) <- expectIdent
      doisPontos <- accept TkColon
      anot <- if doisPontos then Just <$> pType else pure Nothing
      pure (Param p nome anot)

-- | DefClasse ::= 'class' IDENT [ '(' IDENT ')' ] ':' BlocoClasse
pClassDef :: Parser ClassDef
pClassDef = do
  t <- expect TkClass
  (_, nome) <- expectIdent
  super <- pOptSuper
  _ <- expect TkColon
  (campos, metodos) <- pClassBlock
  pure (ClassDef (tokPos t) nome super campos metodos)

pOptSuper :: Parser (Maybe Name)
pOptSuper = do
  abre <- accept TkLParen
  if not abre then pure Nothing else do
    (_, nome) <- expectIdent
    _ <- expect TkRParen
    pure (Just nome)

-- | BlocoClasse ::= NEWLINE INDENT { Campo } { DefFuncao } DEDENT
--
-- Os campos precedem os metodos (secao 6.3). Uma classe sem campos
-- nem metodos tem como corpo o comando pass, que e descartado.
pClassBlock :: Parser ([Field], [FunDef])
pClassBlock = do
  _ <- expect TkNewline
  _ <- expect TkIndent
  campos <- while ehCampo pField
  metodos <- while ((== TkDef) <$> peekLexeme) pFunDef
  passo <- accept TkPass
  _ <- if passo then () <$ expect TkNewline else pure ()
  _ <- expect TkDedent
  pure (campos, metodos)
  where
    -- Um campo comeca com IDENT seguido de ':'.
    ehCampo = do
      (a, b) <- peekTwo
      pure (isIdent a && b == TkColon)

-- | Repete o parser enquanto a condicao, que nao consome, valer.
while :: Parser Bool -> Parser a -> Parser [a]
while cond p = do
  continua <- cond
  if continua then (:) <$> p <*> while cond p else pure []

-- | Campo ::= IDENT ':' Tipo NEWLINE
pField :: Parser Field
pField = do
  (p, nome) <- expectIdent
  _ <- expect TkColon
  t <- pType
  _ <- expect TkNewline
  pure (Field p nome t)

-- | Bloco ::= NEWLINE INDENT Comando { Comando } DEDENT
pBlock :: Parser Block
pBlock = do
  _ <- expect TkNewline
  _ <- expect TkIndent
  primeiro <- pStmt
  resto <- while maisComandos pStmt
  _ <- expect TkDedent
  pure (primeiro : resto)
  where
    maisComandos = (`notElem` [TkDedent, TkEOF]) <$> peekLexeme

-- Comandos -----------------------------------------------------------

-- | Comando ::= ComandoSimples NEWLINE | ComandoComposto | DefFuncao
pStmt :: Parser Stmt
pStmt = do
  l <- peekLexeme
  case l of
    TkIf    -> pIf
    TkWhile -> pWhile
    TkFor   -> pFor
    TkDef   -> SFunDef <$> pFunDef
    _       -> pSimpleStmt

-- | ComandoIf ::= 'if' Expr ':' Bloco { 'elif' Expr ':' Bloco } [ 'else' ':' Bloco ]
pIf :: Parser Stmt
pIf = do
  t <- expect TkIf
  cond <- pExpr
  _ <- expect TkColon
  entao <- pBlock
  elifs <- while ((== TkElif) <$> peekLexeme) pElif
  senao <- pElse
  pure (SIf (tokPos t) cond entao elifs senao)

pElif :: Parser (Pos, Expr, Block)
pElif = do
  t <- expect TkElif
  cond <- pExpr
  _ <- expect TkColon
  corpo <- pBlock
  pure (tokPos t, cond, corpo)

pElse :: Parser Block
pElse = do
  temElse <- accept TkElse
  if not temElse then pure [] else expect TkColon >> pBlock

-- | ComandoWhile ::= 'while' Expr ':' Bloco
pWhile :: Parser Stmt
pWhile = do
  t <- expect TkWhile
  cond <- pExpr
  _ <- expect TkColon
  SWhile (tokPos t) cond <$> pBlock

-- | ComandoFor ::= 'for' IDENT 'in' Expr ':' Bloco
pFor :: Parser Stmt
pFor = do
  t <- expect TkFor
  (_, v) <- expectIdent
  _ <- expect TkIn
  it <- pExpr
  _ <- expect TkColon
  SFor (tokPos t) v it <$> pBlock

-- | ComandoSimples seguido de NEWLINE.
pSimpleStmt :: Parser Stmt
pSimpleStmt = do
  s <- pSimple
  _ <- expect TkNewline
  pure s

pSimple :: Parser Stmt
pSimple = do
  t <- peekToken
  let p = tokPos t
  case tokLexeme t of
    TkPass     -> SPass p <$ advance
    TkBreak    -> SBreak p <$ advance
    TkContinue -> SContinue p <$ advance
    TkReturn   -> do
      _ <- advance
      fim <- (== TkNewline) <$> peekLexeme
      if fim then pure (SReturn p Nothing) else SReturn p . Just <$> pExpr
    _ -> pAssignOrExpr

-- | Atribuicao ou expressao usada como comando.
--
-- A declaracao com anotacao, @nome: tipo = expr@, e reconhecida pelos
-- dois primeiros tokens: IDENT seguido de ':'. Nos demais casos
-- reconhece-se uma expressao e, se vier em seguida um operador de
-- atribuicao, a expressao ja construida e convertida em alvo.
pAssignOrExpr :: Parser Stmt
pAssignOrExpr = do
  (a, b) <- peekTwo
  if isIdent a && b == TkColon then pVarDecl else pAssignTail

-- | Atribuicao ::= IDENT ':' Tipo '=' Expr
pVarDecl :: Parser Stmt
pVarDecl = do
  (p, nome) <- expectIdent
  _ <- expect TkColon
  t <- pType
  _ <- expect TkAssign
  SVarDecl p nome t <$> pExpr

-- | Expressao seguida, opcionalmente, de '=' ou de um operador
-- aumentado.
pAssignTail :: Parser Stmt
pAssignTail = do
  e <- pExpr
  l <- peekLexeme
  case l of
    TkAssign -> do
      alvo <- toLValue e
      _ <- advance
      SAssign (exprStart e) alvo <$> pExpr
    _ | Just op <- lookup l augOps -> do
      alvo <- toLValue e
      _ <- advance
      SAugAssign (exprStart e) alvo op <$> pExpr
    _ -> pure (SExpr (exprStart e) e)

-- | Alvos validos sao nome, acesso indexado e acesso a campo (secao 7.2
-- da especificacao). O erro aponta a expressao recusada.
toLValue :: Expr -> Parser LValue
toLValue e = case e of
  EVar p n     -> pure (LVar p n)
  EIndex p b i -> pure (LIndex p b i)
  EField p b n -> pure (LField p b n)
  _            -> failAt (exprStart e) "invalid assignment target"

augOps :: [(Lexeme, AugOp)]
augOps =
  [ (TkPlusEq, AddAssign), (TkMinusEq, SubAssign)
  , (TkStarEq, MulAssign), (TkSlashEq, DivAssign)
  , (TkDSlashEq, FloorDivAssign), (TkPercentEq, ModAssign)
  ]

-- Tipos --------------------------------------------------------------

-- | Tipo ::= TipoFuncao | TipoAtomico
pType :: Parser Type
pType = do
  t <- peekToken
  let p = tokPos t
  case tokLexeme t of
    TkIntKw   -> TyInt p <$ advance
    TkFloatKw -> TyFloat p <$ advance
    TkBoolKw  -> TyBool p <$ advance
    TkStrKw   -> TyStr p <$ advance
    TkNoneKw  -> TyNone p <$ advance
    -- TipoFuncao ::= '(' [ Tipo { ',' Tipo } ] ')' '->' Tipo
    TkLParen -> do
      _ <- advance
      args <- commaSepUntil TkRParen pType
      _ <- expect TkRParen
      _ <- expect TkArrow
      TyFun p args <$> pType
    TkIdent n -> do
      _ <- advance
      colchete <- peekToken
      if tokLexeme colchete /= TkLBracket then pure (nameType p n) else do
        -- So list recebe argumento de tipo: list[T].
        _ <- advance
        elemT <- pType
        _ <- expect TkRBracket
        if n == "list"
          then pure (TyList p elemT)
          else failAt (tokPos colchete) ("unexpected '[' after type '" ++ n ++ "'")
    _ -> expected "type"

-- | Identificadores iniciados por maiuscula sao tipos de classe; os
-- demais sao variaveis de tipo (secao 3.5).
nameType :: Pos -> Name -> Type
nameType p n = case n of
  (c : _) | isUpper c -> TyClass p n
  _                   -> TyVar p n

-- Expressoes ---------------------------------------------------------

-- | Expr ::= 'lambda' [ IDENT { ',' IDENT } ] ':' Expr
--          | ExprOu [ 'if' ExprOu 'else' Expr ]
--
-- A lambda fica neste nivel, e nao entre os atomos: o corpo vai ate o
-- fim da expressao, e lambda x: x + 1 tem uma leitura so. Como operando
-- de outro operador ela precisa de parenteses. Os parametros nao tem
-- anotacao e podem faltar, como em lambda: 42 (secao 6.11).
--
-- A expressao condicional associa a direita: o ramo else e uma Expr
-- completa (secao 6.10 da especificacao).
pExpr :: Parser Expr
pExpr = do
  t <- peekToken
  case tokLexeme t of
    TkLambda -> do
      _ <- advance
      ps <- map snd <$> commaSepUntil TkColon expectIdent
      _ <- expect TkColon
      ELambda (tokPos t) ps <$> pExpr
    _ -> do
      e <- pOr
      condicional <- accept TkIf
      if not condicional then pure e else do
        cond <- pOr
        _ <- expect TkElse
        ECond (exprStart e) e cond <$> pExpr

-- | ExprOu ::= ExprE { 'or' ExprE }
pOr :: Parser Expr
pOr = chainLeft pAnd [(TkOr, Or)]

-- | ExprE ::= ExprNao { 'and' ExprNao }
pAnd :: Parser Expr
pAnd = chainLeft pNot [(TkAnd, And)]

-- | ExprNao ::= 'not' ExprNao | ExprRel
pNot :: Parser Expr
pNot = do
  t <- peekToken
  case tokLexeme t of
    TkNot -> advance >> EUnOp (tokPos t) Not <$> pNot
    _     -> pRel

-- | ExprRel ::= ExprAdit [ OpRel ExprAdit ]
--
-- No maximo um operador relacional: a < b < c e erro de sintaxe.
pRel :: Parser Expr
pRel = do
  left <- pAdd
  t <- peekToken
  case lookup (tokLexeme t) relOps of
    Nothing -> pure left
    Just op -> advance >> EBinOp (tokPos t) op left <$> pAdd

-- | ExprAdit ::= ExprMult { ( '+' | '-' ) ExprMult }
pAdd :: Parser Expr
pAdd = chainLeft pMul [(TkPlus, Add), (TkMinus, Sub)]

-- | ExprMult ::= ExprPot { ( '*' | '/' | '//' | '%' ) ExprPot }
pMul :: Parser Expr
pMul = chainLeft pPow mulOps

-- | ExprPot ::= ExprUnaria [ '**' ExprPot ]
--
-- Recursao a direita no segundo operando: ** associa a direita. Como
-- os operandos sao ExprUnaria, o menos unario liga mais forte que **
-- (secao 6.3): -2 ** 2 e (-2) ** 2, e 2 ** -1 e aceito.
pPow :: Parser Expr
pPow = do
  base <- pUnary
  t <- peekToken
  pot <- accept TkDStar
  if pot then EBinOp (tokPos t) Pow base <$> pPow else pure base

-- | ExprUnaria ::= '-' ExprUnaria | ExprPosfixa
pUnary :: Parser Expr
pUnary = do
  t <- peekToken
  case tokLexeme t of
    TkMinus -> advance >> EUnOp (tokPos t) Neg <$> pUnary
    _       -> pPostfix

-- | ExprPosfixa ::= Atomo { Posfixo }
--
-- Posfixo ::= '(' [ Args ] ')' | '[' Expr ']' | '.' IDENT
-- A repeticao constroi a arvore associando a esquerda, de modo que
-- obj.lista[0].campo() seja aninhado na ordem de escrita.
pPostfix :: Parser Expr
pPostfix = pAtom >>= go
  where
    go acc = do
      t <- peekToken
      let p = tokPos t
      case tokLexeme t of
        TkLParen -> do
          _ <- advance
          args <- commaSepUntil TkRParen pExpr
          _ <- expect TkRParen
          go (ECall p acc args)
        TkLBracket -> do
          _ <- advance
          idx <- pExpr
          _ <- expect TkRBracket
          go (EIndex p acc idx)
        TkDot -> do
          _ <- advance
          (_, n) <- expectIdent
          go (EField p acc n)
        _ -> pure acc

-- | Atomo, conforme a producao de mesmo nome na GLC.
pAtom :: Parser Expr
pAtom = do
  t <- peekToken
  let p = tokPos t
  case tokLexeme t of
    TkIntLit n   -> EInt p n <$ advance
    TkFloatLit d -> EFloat p d <$ advance
    TkStrLit s   -> EStr p s <$ advance
    TkBoolLit b  -> EBool p b <$ advance
    TkNoneKw     -> ENone p <$ advance
    TkSelf       -> ESelf p <$ advance
    TkIdent n    -> EVar p n <$ advance
    -- int, float, str e bool sao palavras reservadas de tipo e tambem
    -- funcoes primitivas de conversao (secao 8 da especificacao). Em
    -- posicao de expressao valem como nome de funcao; a chamada e
    -- montada pelo nivel posfixo.
    TkIntKw      -> EVar p "int" <$ advance
    TkFloatKw    -> EVar p "float" <$ advance
    TkStrKw      -> EVar p "str" <$ advance
    TkBoolKw     -> EVar p "bool" <$ advance
    TkLBracket -> do
      _ <- advance
      es <- commaSepUntil TkRBracket pExpr
      _ <- expect TkRBracket
      pure (EList p es)
    TkLParen -> do
      _ <- advance
      e <- pExpr
      _ <- expect TkRParen
      pure e
    _ -> expected "expression"

-- | Reconhece uma cadeia associativa a esquerda: proximo { op proximo }.
chainLeft :: Parser Expr -> [(Lexeme, BinOp)] -> Parser Expr
chainLeft next ops = next >>= go
  where
    go acc = do
      t <- peekToken
      case lookup (tokLexeme t) ops of
        Nothing -> pure acc
        Just op -> do
          _ <- advance
          right <- next
          go (EBinOp (tokPos t) op acc right)

isIdent :: Lexeme -> Bool
isIdent (TkIdent _) = True
isIdent _           = False

relOps :: [(Lexeme, BinOp)]
relOps =
  [ (TkEqEq, Eq), (TkNeq, Neq), (TkLt, Lt)
  , (TkLe, Le), (TkGt, Gt), (TkGe, Ge)
  ]

mulOps :: [(Lexeme, BinOp)]
mulOps =
  [ (TkStar, Mul), (TkSlash, Div)
  , (TkDSlash, FloorDiv), (TkPercent, Mod)
  ]
