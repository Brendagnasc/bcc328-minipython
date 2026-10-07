-- | Analisador descendente recursivo: comandos e declaracoes.
--
-- Cada funcao corresponde a um nao terminal da GLC proposta em
-- docs/gramatica.md. O ponto de entrada e pProgram.
module MiniPython.Frontend.Parser.RecursiveDescent
  ( parseProgram
  ) where

import Data.Char (isUpper)
import MiniPython.Frontend.Lexer.Token
import MiniPython.Frontend.Parser.Core
import MiniPython.Frontend.Parser.Expr
import MiniPython.Frontend.Syntax.MiniPythonSyntax

-- | Reconhece um programa completo a partir da lista de tokens.
parseProgram :: [Token] -> Either ParseError Program
parseProgram = runFully pProgram

-- | Programa ::= { Decl } EOF
pProgram :: Parser Program
pProgram = Program <$> many0 maisDecls pDecl
  where
    maisDecls = do
      t <- peek
      pure (tokLexeme t /= TEOF)

-- | Decl ::= DefFuncao | DefClasse | Comando
pDecl :: Parser Decl
pDecl = do
  t <- peek
  case tokLexeme t of
    TDef   -> DFun <$> pFunDef
    TClass -> DClass <$> pClassDef
    _      -> DStmt <$> pStmt

-- | DefFuncao ::= 'def' IDENT '(' [ Params ] ')' [ '->' Tipo ] ':' Bloco
pFunDef :: Parser FunDef
pFunDef = do
  t <- expect TDef "def" "definicao de funcao"
  (_, nome) <- expectIdent "definicao de funcao"
  _ <- expect TLParen "(" "definicao de funcao"
  ps <- pParams
  _ <- expect TRParen ")" "definicao de funcao"
  ret <- pOptReturn
  _ <- expect TColon ":" "definicao de funcao"
  corpo <- pBlock
  pure (FunDef (tokPos t) nome ps ret corpo)

pOptReturn :: Parser (Maybe Type)
pOptReturn = do
  m <- accept TArrow
  case m of
    Nothing -> pure Nothing
    Just _  -> Just <$> pType

-- | Params ::= Param { ',' Param }, possivelmente vazio.
pParams :: Parser [Param]
pParams = do
  vazio <- check TRParen
  if vazio then pure [] else sepBy1 TComma pParam

-- | Param ::= IDENT [ ':' Tipo ]
--
-- Em metodos, self aparece como primeiro parametro. Como self e
-- palavra reservada (secao 4.2), ele nao casa com IDENT e e tratado
-- a parte.
pParam :: Parser Param
pParam = do
  t <- peek
  case tokLexeme t of
    TSelf -> do
      _ <- advance
      pure (Param (tokPos t) "self" Nothing)
    _ -> do
      (p, nome) <- expectIdent "parametro"
      anot <- pOptAnnotation
      pure (Param p nome anot)

pOptAnnotation :: Parser (Maybe Type)
pOptAnnotation = do
  m <- accept TColon
  case m of
    Nothing -> pure Nothing
    Just _  -> Just <$> pType

-- | DefClasse ::= 'class' IDENT [ '(' IDENT ')' ] ':' BlocoClasse
pClassDef :: Parser ClassDef
pClassDef = do
  t <- expect TClass "class" "definicao de classe"
  (_, nome) <- expectIdent "definicao de classe"
  super <- pOptSuper
  _ <- expect TColon ":" "definicao de classe"
  (campos, metodos) <- pClassBlock
  pure (ClassDef (tokPos t) nome super campos metodos)

pOptSuper :: Parser (Maybe Name)
pOptSuper = do
  m <- accept TLParen
  case m of
    Nothing -> pure Nothing
    Just _ -> do
      (_, nome) <- expectIdent "superclasse"
      _ <- expect TRParen ")" "superclasse"
      pure (Just nome)

-- | BlocoClasse ::= NEWLINE INDENT { Campo } { DefFuncao } DEDENT
--
-- Os campos precedem os metodos (secao 6.3). Uma classe sem campos
-- nem metodos tem como corpo o comando pass, que e descartado.
pClassBlock :: Parser ([Field], [FunDef])
pClassBlock = do
  _ <- expect TNewline "fim de linha" "corpo de classe"
  _ <- expect TIndent "aumento de indentacao" "corpo de classe"
  campos <- many0 ehCampo pField
  metodos <- many0 (check TDef) pFunDef
  _ <- pSkipPass
  _ <- expect TDedent "fim de bloco" "corpo de classe"
  pure (campos, metodos)
  where
    -- Um campo comeca com IDENT seguido de ':'; um metodo comeca
    -- com def. O segundo token distingue campo de comando solto.
    ehCampo = do
      t <- peek
      case tokLexeme t of
        TIdent _ -> do
          mnext <- peekAt 1
          pure (fmap tokLexeme mnext == Just TColon)
        _ -> pure False

-- | Consome um pass isolado no corpo de uma classe vazia.
pSkipPass :: Parser ()
pSkipPass = do
  m <- accept TPass
  case m of
    Nothing -> pure ()
    Just _  -> () <$ expect TNewline "fim de linha" "corpo de classe"

-- | Campo ::= IDENT ':' Tipo NEWLINE
pField :: Parser Field
pField = do
  (p, nome) <- expectIdent "campo de classe"
  _ <- expect TColon ":" "campo de classe"
  t <- pType
  _ <- expect TNewline "fim de linha" "campo de classe"
  pure (Field p nome t)

-- | Bloco ::= NEWLINE INDENT Comando { Comando } DEDENT
pBlock :: Parser Block
pBlock = do
  _ <- expect TNewline "fim de linha" "bloco"
  _ <- expect TIndent "aumento de indentacao" "bloco"
  primeiro <- pStmt
  resto <- many0 maisStmts pStmt
  _ <- expect TDedent "fim de bloco" "bloco"
  pure (primeiro : resto)
  where
    maisStmts = do
      t <- peek
      pure (tokLexeme t /= TDedent && tokLexeme t /= TEOF)

-- | Comando ::= ComandoSimples NEWLINE | ComandoComposto
pStmt :: Parser Stmt
pStmt = do
  t <- peek
  case tokLexeme t of
    TIf    -> pIf
    TWhile -> pWhile
    TFor   -> pFor
    _      -> pSimpleStmt

-- | ComandoIf ::= 'if' Expr ':' Bloco { 'elif' ... } [ 'else' ... ]
pIf :: Parser Stmt
pIf = do
  t <- expect TIf "if" "comando if"
  cond <- pExpr
  _ <- expect TColon ":" "comando if"
  entao <- pBlock
  elifs <- many0 (check TElif) pElif
  senao <- pElse
  pure (SIf (tokPos t) cond entao elifs senao)

pElif :: Parser (Pos, Expr, Block)
pElif = do
  t <- expect TElif "elif" "clausula elif"
  cond <- pExpr
  _ <- expect TColon ":" "clausula elif"
  corpo <- pBlock
  pure (tokPos t, cond, corpo)

pElse :: Parser Block
pElse = do
  m <- accept TElse
  case m of
    Nothing -> pure []
    Just _ -> do
      _ <- expect TColon ":" "clausula else"
      pBlock

pWhile :: Parser Stmt
pWhile = do
  t <- expect TWhile "while" "comando while"
  cond <- pExpr
  _ <- expect TColon ":" "comando while"
  corpo <- pBlock
  pure (SWhile (tokPos t) cond corpo)

-- | ComandoFor ::= 'for' IDENT 'in' Expr ':' Bloco
pFor :: Parser Stmt
pFor = do
  t <- expect TFor "for" "comando for"
  (_, v) <- expectIdent "comando for"
  _ <- expect TIn "in" "comando for"
  it <- pExpr
  _ <- expect TColon ":" "comando for"
  corpo <- pBlock
  pure (SFor (tokPos t) v it corpo)

-- | ComandoSimples seguido de NEWLINE.
pSimpleStmt :: Parser Stmt
pSimpleStmt = do
  s <- pSimple
  _ <- expect TNewline "fim de linha" "comando"
  pure s

pSimple :: Parser Stmt
pSimple = do
  t <- peek
  let p = tokPos t
  case tokLexeme t of
    TPass     -> advance >> pure (SPass p)
    TBreak    -> advance >> pure (SBreak p)
    TContinue -> advance >> pure (SContinue p)
    TReturn   -> do
      _ <- advance
      fim <- check TNewline
      if fim then pure (SReturn p Nothing) else SReturn p . Just <$> pExpr
    _ -> pAssignOrExpr

-- | Atribuicao ou expressao usada como comando.
--
-- A declaracao com anotacao, @nome: tipo = expr@, e identificada
-- olhando dois tokens a frente: IDENT seguido de ':'. Nos demais
-- casos reconhece-se uma expressao e, se vier em seguida um operador
-- de atribuicao, a expressao ja construida e convertida em alvo.
pAssignOrExpr :: Parser Stmt
pAssignOrExpr = do
  ehDecl <- pIsVarDecl
  if ehDecl then pVarDecl else pAssignTail

pIsVarDecl :: Parser Bool
pIsVarDecl = do
  t <- peek
  case tokLexeme t of
    TIdent _ -> do
      mnext <- peekAt 1
      pure (fmap tokLexeme mnext == Just TColon)
    _ -> pure False

-- | Atribuicao ::= IDENT ':' Tipo '=' Expr
pVarDecl :: Parser Stmt
pVarDecl = do
  (p, nome) <- expectIdent "declaracao de variavel"
  _ <- expect TColon ":" "declaracao de variavel"
  t <- pType
  _ <- expect TAssign "=" "declaracao de variavel"
  SVarDecl p nome t <$> pExpr

-- | Expressao seguida, opcionalmente, de '=' ou de um operador
-- aumentado.
pAssignTail :: Parser Stmt
pAssignTail = do
  e <- pExpr
  t <- peek
  case tokLexeme t of
    TAssign -> do
      _ <- advance
      alvo <- toLValue e
      SAssign (exprPos e) alvo <$> pExpr
    lx | Just op <- lookup lx augOps -> do
      _ <- advance
      alvo <- toLValue e
      SAugAssign (exprPos e) alvo op <$> pExpr
    _ -> pure (SExpr (exprPos e) e)

-- | Converte uma expressao ja reconhecida em alvo de atribuicao.
-- Alvos validos sao nome, acesso indexado e acesso a campo
-- (secao 7.2 da especificacao).
toLValue :: Expr -> Parser LValue
toLValue e = case e of
  EVar p n      -> pure (LVar p n)
  EIndex p b i  -> pure (LIndex p b i)
  EField p b n  -> pure (LField p b n)
  _ -> failWith "um alvo de atribuicao valido" "atribuicao"

augOps :: [(Lexeme, AugOp)]
augOps =
  [ (TPlusAssign, AddAssign), (TMinusAssign, SubAssign)
  , (TStarAssign, MulAssign), (TSlashAssign, DivAssign)
  , (TDoubleSlashAssign, FloorDivAssign), (TPercentAssign, ModAssign)
  ]

-- | Tipo ::= TipoFuncao | TipoAtomico
pType :: Parser Type
pType = do
  t <- peek
  let p = tokPos t
  case tokLexeme t of
    TInt   -> advance >> pure (TyInt p)
    TFloat -> advance >> pure (TyFloat p)
    TBool  -> advance >> pure (TyBool p)
    TStr   -> advance >> pure (TyStr p)
    TNone  -> advance >> pure (TyNone p)
    TLParen -> do
      _ <- advance
      args <- pTypeListUntil TRParen
      _ <- expect TRParen ")" "tipo de funcao"
      _ <- expect TArrow "->" "tipo de funcao"
      TyFun p args <$> pType
    TIdent "list" -> do
      _ <- advance
      _ <- expect TLBracket "[" "tipo lista"
      elemT <- pType
      _ <- expect TRBracket "]" "tipo lista"
      pure (TyList p elemT)
    -- Identificadores iniciados por maiuscula sao tipos de classe;
    -- os demais sao variaveis de tipo (secao 3.5).
    TIdent n
      | not (null n) && isUpper (head n) -> advance >> pure (TyClass p n)
      | otherwise -> advance >> pure (TyVar p n)
    _ -> failWith "um tipo" "anotacao de tipo"

pTypeListUntil :: Lexeme -> Parser [Type]
pTypeListUntil close = do
  vazio <- check close
  if vazio then pure [] else sepBy1 TComma pType
