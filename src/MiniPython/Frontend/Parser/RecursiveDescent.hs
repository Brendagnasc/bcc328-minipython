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
      pure (tokLexeme t /= TkEOF)

-- | Decl ::= DefFuncao | DefClasse | Comando
pDecl :: Parser Decl
pDecl = do
  t <- peek
  case tokLexeme t of
    TkDef   -> DFun <$> pFunDef
    TkClass -> DClass <$> pClassDef
    _      -> DStmt <$> pStmt

-- | DefFuncao ::= 'def' IDENT '(' [ Params ] ')' [ '->' Tipo ] ':' Bloco
pFunDef :: Parser FunDef
pFunDef = do
  t <- expect TkDef "def" "definicao de funcao"
  (_, nome) <- expectIdent "definicao de funcao"
  _ <- expect TkLParen "(" "definicao de funcao"
  ps <- pParams
  _ <- expect TkRParen ")" "definicao de funcao"
  ret <- pOptReturn
  _ <- expect TkColon ":" "definicao de funcao"
  corpo <- pBlock
  pure (FunDef (tokPos t) nome ps ret corpo)

pOptReturn :: Parser (Maybe Type)
pOptReturn = do
  m <- accept TkArrow
  case m of
    Nothing -> pure Nothing
    Just _  -> Just <$> pType

-- | Params ::= Param { ',' Param }, possivelmente vazio.
pParams :: Parser [Param]
pParams = do
  vazio <- check TkRParen
  if vazio then pure [] else sepBy1 TkComma pParam

-- | Param ::= IDENT [ ':' Tipo ]
--
-- Em metodos, self aparece como primeiro parametro. Como self e
-- palavra reservada (secao 4.2), ele nao casa com IDENT e e tratado
-- a parte.
pParam :: Parser Param
pParam = do
  t <- peek
  case tokLexeme t of
    TkSelf -> do
      _ <- advance
      pure (Param (tokPos t) "self" Nothing)
    _ -> do
      (p, nome) <- expectIdent "parametro"
      anot <- pOptAnnotation
      pure (Param p nome anot)

pOptAnnotation :: Parser (Maybe Type)
pOptAnnotation = do
  m <- accept TkColon
  case m of
    Nothing -> pure Nothing
    Just _  -> Just <$> pType

-- | DefClasse ::= 'class' IDENT [ '(' IDENT ')' ] ':' BlocoClasse
pClassDef :: Parser ClassDef
pClassDef = do
  t <- expect TkClass "class" "definicao de classe"
  (_, nome) <- expectIdent "definicao de classe"
  super <- pOptSuper
  _ <- expect TkColon ":" "definicao de classe"
  (campos, metodos) <- pClassBlock
  pure (ClassDef (tokPos t) nome super campos metodos)

pOptSuper :: Parser (Maybe Name)
pOptSuper = do
  m <- accept TkLParen
  case m of
    Nothing -> pure Nothing
    Just _ -> do
      (_, nome) <- expectIdent "superclasse"
      _ <- expect TkRParen ")" "superclasse"
      pure (Just nome)

-- | BlocoClasse ::= NEWLINE INDENT { Campo } { DefFuncao } DEDENT
--
-- Os campos precedem os metodos (secao 6.3). Uma classe sem campos
-- nem metodos tem como corpo o comando pass, que e descartado.
pClassBlock :: Parser ([Field], [FunDef])
pClassBlock = do
  _ <- expect TkNewline "fim de linha" "corpo de classe"
  _ <- expect TkIndent "aumento de indentacao" "corpo de classe"
  campos <- many0 ehCampo pField
  metodos <- many0 (check TkDef) pFunDef
  _ <- pSkipPass
  _ <- expect TkDedent "fim de bloco" "corpo de classe"
  pure (campos, metodos)
  where
    -- Um campo comeca com IDENT seguido de ':'; um metodo comeca
    -- com def. O segundo token distingue campo de comando solto.
    ehCampo = do
      t <- peek
      case tokLexeme t of
        TkIdent _ -> do
          mnext <- peekAt 1
          pure (fmap tokLexeme mnext == Just TkColon)
        _ -> pure False

-- | Consome um pass isolado no corpo de uma classe vazia.
pSkipPass :: Parser ()
pSkipPass = do
  m <- accept TkPass
  case m of
    Nothing -> pure ()
    Just _  -> () <$ expect TkNewline "fim de linha" "corpo de classe"

-- | Campo ::= IDENT ':' Tipo NEWLINE
pField :: Parser Field
pField = do
  (p, nome) <- expectIdent "campo de classe"
  _ <- expect TkColon ":" "campo de classe"
  t <- pType
  _ <- expect TkNewline "fim de linha" "campo de classe"
  pure (Field p nome t)

-- | Bloco ::= NEWLINE INDENT Comando { Comando } DEDENT
pBlock :: Parser Block
pBlock = do
  _ <- expect TkNewline "fim de linha" "bloco"
  _ <- expect TkIndent "aumento de indentacao" "bloco"
  primeiro <- pStmt
  resto <- many0 maisStmts pStmt
  _ <- expect TkDedent "fim de bloco" "bloco"
  pure (primeiro : resto)
  where
    maisStmts = do
      t <- peek
      pure (tokLexeme t /= TkDedent && tokLexeme t /= TkEOF)

-- | Comando ::= ComandoSimples NEWLINE | ComandoComposto
pStmt :: Parser Stmt
pStmt = do
  t <- peek
  case tokLexeme t of
    TkIf    -> pIf
    TkWhile -> pWhile
    TkFor   -> pFor
    _      -> pSimpleStmt

-- | ComandoIf ::= 'if' Expr ':' Bloco { 'elif' ... } [ 'else' ... ]
pIf :: Parser Stmt
pIf = do
  t <- expect TkIf "if" "comando if"
  cond <- pExpr
  _ <- expect TkColon ":" "comando if"
  entao <- pBlock
  elifs <- many0 (check TkElif) pElif
  senao <- pElse
  pure (SIf (tokPos t) cond entao elifs senao)

pElif :: Parser (Pos, Expr, Block)
pElif = do
  t <- expect TkElif "elif" "clausula elif"
  cond <- pExpr
  _ <- expect TkColon ":" "clausula elif"
  corpo <- pBlock
  pure (tokPos t, cond, corpo)

pElse :: Parser Block
pElse = do
  m <- accept TkElse
  case m of
    Nothing -> pure []
    Just _ -> do
      _ <- expect TkColon ":" "clausula else"
      pBlock

pWhile :: Parser Stmt
pWhile = do
  t <- expect TkWhile "while" "comando while"
  cond <- pExpr
  _ <- expect TkColon ":" "comando while"
  corpo <- pBlock
  pure (SWhile (tokPos t) cond corpo)

-- | ComandoFor ::= 'for' IDENT 'in' Expr ':' Bloco
pFor :: Parser Stmt
pFor = do
  t <- expect TkFor "for" "comando for"
  (_, v) <- expectIdent "comando for"
  _ <- expect TkIn "in" "comando for"
  it <- pExpr
  _ <- expect TkColon ":" "comando for"
  corpo <- pBlock
  pure (SFor (tokPos t) v it corpo)

-- | ComandoSimples seguido de NEWLINE.
pSimpleStmt :: Parser Stmt
pSimpleStmt = do
  s <- pSimple
  _ <- expect TkNewline "fim de linha" "comando"
  pure s

pSimple :: Parser Stmt
pSimple = do
  t <- peek
  let p = tokPos t
  case tokLexeme t of
    TkPass     -> advance >> pure (SPass p)
    TkBreak    -> advance >> pure (SBreak p)
    TkContinue -> advance >> pure (SContinue p)
    TkReturn   -> do
      _ <- advance
      fim <- check TkNewline
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
    TkIdent _ -> do
      mnext <- peekAt 1
      pure (fmap tokLexeme mnext == Just TkColon)
    _ -> pure False

-- | Atribuicao ::= IDENT ':' Tipo '=' Expr
pVarDecl :: Parser Stmt
pVarDecl = do
  (p, nome) <- expectIdent "declaracao de variavel"
  _ <- expect TkColon ":" "declaracao de variavel"
  t <- pType
  _ <- expect TkAssign "=" "declaracao de variavel"
  SVarDecl p nome t <$> pExpr

-- | Expressao seguida, opcionalmente, de '=' ou de um operador
-- aumentado.
pAssignTail :: Parser Stmt
pAssignTail = do
  e <- pExpr
  t <- peek
  case tokLexeme t of
    TkAssign -> do
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
  -- O erro e reportado na posicao da expressao recusada, e nao na do
  -- proximo token, porque e ela que nao constitui alvo valido.
  _ -> failAt (exprPos e) "esta expressao" "um alvo de atribuicao valido" "atribuicao"

augOps :: [(Lexeme, AugOp)]
augOps =
  [ (TkPlusEq, AddAssign), (TkMinusEq, SubAssign)
  , (TkStarEq, MulAssign), (TkSlashEq, DivAssign)
  , (TkDSlashEq, FloorDivAssign), (TkPercentEq, ModAssign)
  ]

-- | Tipo ::= TipoFuncao | TipoAtomico
pType :: Parser Type
pType = do
  t <- peek
  let p = tokPos t
  case tokLexeme t of
    TkIntKw   -> advance >> pure (TyInt p)
    TkFloatKw -> advance >> pure (TyFloat p)
    TkBoolKw  -> advance >> pure (TyBool p)
    TkStrKw   -> advance >> pure (TyStr p)
    TkNoneKw  -> advance >> pure (TyNone p)
    TkLParen -> do
      _ <- advance
      args <- pTypeListUntil TkRParen
      _ <- expect TkRParen ")" "tipo de funcao"
      _ <- expect TkArrow "->" "tipo de funcao"
      TyFun p args <$> pType
    TkIdent "list" -> do
      _ <- advance
      _ <- expect TkLBracket "[" "tipo lista"
      elemT <- pType
      _ <- expect TkRBracket "]" "tipo lista"
      pure (TyList p elemT)
    -- Identificadores iniciados por maiuscula sao tipos de classe;
    -- os demais sao variaveis de tipo (secao 3.5).
    TkIdent n
      | not (null n) && isUpper (head n) -> advance >> pure (TyClass p n)
      | otherwise -> advance >> pure (TyVar p n)
    _ -> failWith "um tipo" "anotacao de tipo"

pTypeListUntil :: Lexeme -> Parser [Type]
pTypeListUntil close = do
  vazio <- check close
  if vazio then pure [] else sepBy1 TkComma pType
