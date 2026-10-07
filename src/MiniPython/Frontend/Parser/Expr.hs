-- | Analisador descendente recursivo: nivel de expressoes.
--
-- Cada funcao deste modulo corresponde a um nao terminal da GLC
-- proposta em docs/gramatica.md, na mesma ordem de precedencia:
-- Expr, ExprOu, ExprE, ExprNao, ExprRel, ExprAdit, ExprMult,
-- ExprUnaria, ExprPot, ExprPosfixa e Atomo.
module MiniPython.Frontend.Parser.Expr
  ( pExpr
  , pArgs
  ) where

import MiniPython.Frontend.Lexer.Token
import MiniPython.Frontend.Parser.Core
import MiniPython.Frontend.Syntax.MiniPythonSyntax

-- | Expr ::= ExprOu [ 'if' ExprOu 'else' Expr ]
--
-- A expressao condicional associa a direita: o ramo else e uma Expr
-- completa, de modo que a..c if p else d if q else e seja lido como
-- aninhamento a direita (secao 6.10 da especificacao).
pExpr :: Parser Expr
pExpr = do
  e <- pOr
  m <- accept TIf
  case m of
    Nothing -> pure e
    Just t -> do
      cond <- pOr
      _ <- expect TElse "else" "expressao condicional"
      alt <- pExpr
      pure (ECond (tokPos t) e cond alt)

-- | ExprOu ::= ExprE { 'or' ExprE }
pOr :: Parser Expr
pOr = chainLeft pAnd [(TOr, Or)]

-- | ExprE ::= ExprNao { 'and' ExprNao }
pAnd :: Parser Expr
pAnd = chainLeft pNot [(TAnd, And)]

-- | ExprNao ::= 'not' ExprNao | ExprRel
pNot :: Parser Expr
pNot = do
  m <- accept TNot
  case m of
    Just t  -> EUn (tokPos t) Not <$> pNot
    Nothing -> pRel

-- | ExprRel ::= ExprAdit [ OpRel ExprAdit ]
--
-- No maximo um operador relacional: a < b < c e erro de sintaxe,
-- conforme decidido na GLC proposta.
pRel :: Parser Expr
pRel = do
  left <- pAdd
  mop <- peekOp relOps
  case mop of
    Nothing -> pure left
    Just (t, op) -> do
      _ <- advance
      right <- pAdd
      pure (EBin (tokPos t) op left right)

-- | ExprAdit ::= ExprMult { ( '+' | '-' ) ExprMult }
pAdd :: Parser Expr
pAdd = chainLeft pMul [(TPlus, Add), (TMinus, Sub)]

-- | ExprMult ::= ExprUnaria { ( '*' | '/' | '//' | '%' ) ExprUnaria }
pMul :: Parser Expr
pMul = chainLeft pUnary mulOps

-- | ExprUnaria ::= '-' ExprUnaria | ExprPot
--
-- O menos unario liga mais forte que ** (secao 6.3 da especificacao),
-- portanto -2 ** 2 e lido como (-2) ** 2.
pUnary :: Parser Expr
pUnary = do
  m <- accept TMinus
  case m of
    Just t  -> EUn (tokPos t) Neg <$> pUnary
    Nothing -> pPow

-- | ExprPot ::= ExprPosfixa [ '**' ExprPot ]
--
-- Recursao a direita no segundo operando: ** associa a direita.
pPow :: Parser Expr
pPow = do
  base <- pPostfix
  m <- accept TDoubleStar
  case m of
    Nothing -> pure base
    Just t  -> EBin (tokPos t) Pow base <$> pPow

-- | ExprPosfixa ::= Atomo { Posfixo }
--
-- Posfixo ::= '(' [ Args ] ')' | '[' Expr ']' | '.' IDENT
-- A repeticao constroi a arvore associando a esquerda, de modo que
-- obj.lista[0].campo() seja aninhado na ordem de escrita.
pPostfix :: Parser Expr
pPostfix = pAtom >>= go
  where
    go acc = do
      t <- peek
      case tokLexeme t of
        TLParen -> do
          _ <- advance
          args <- pArgsUntil TRParen
          _ <- expect TRParen ")" "chamada de funcao"
          go (ECall (tokPos t) acc args)
        TLBracket -> do
          _ <- advance
          idx <- pExpr
          _ <- expect TRBracket "]" "acesso indexado"
          go (EIndex (tokPos t) acc idx)
        TDot -> do
          _ <- advance
          (_, n) <- expectIdent "acesso a campo"
          go (EField (tokPos t) acc n)
        _ -> pure acc

-- | Args ::= Expr { ',' Expr }
pArgs :: Parser [Expr]
pArgs = sepBy1 TComma pExpr

-- | Lista de argumentos possivelmente vazia, terminada pelo lexema dado.
pArgsUntil :: Lexeme -> Parser [Expr]
pArgsUntil close = do
  vazio <- check close
  if vazio then pure [] else pArgs

-- | Atomo, conforme a producao de mesmo nome na GLC.
pAtom :: Parser Expr
pAtom = do
  t <- peek
  let p = tokPos t
  case tokLexeme t of
    TIntLit n    -> advance >> pure (EInt p n)
    TFloatLit d  -> advance >> pure (EFloat p d)
    TStringLit s -> advance >> pure (EStr p s)
    TBoolLit b   -> advance >> pure (EBool p b)
    TNone        -> advance >> pure (ENone p)
    TSelf        -> advance >> pure (ESelf p)
    TIdent n     -> advance >> pure (EVar p n)
    TLBracket    -> do
      _ <- advance
      es <- pArgsUntil TRBracket
      _ <- expect TRBracket "]" "literal de lista"
      pure (EList p es)
    TLambda -> do
      _ <- advance
      ps <- pLambdaParams
      _ <- expect TColon ":" "expressao lambda"
      ELambda p ps <$> pExpr
    TLParen -> do
      _ <- advance
      e <- pExpr
      _ <- expect TRParen ")" "expressao entre parenteses"
      pure e
    _ -> failWith "uma expressao" "expressao"

-- | Parametros de lambda: nomes sem anotacao de tipo, possivelmente
-- nenhum, como em @lambda: 42@ (secao 6.11 da especificacao).
pLambdaParams :: Parser [Name]
pLambdaParams = do
  semParams <- check TColon
  if semParams
    then pure []
    else map snd <$> sepBy1 TComma (expectIdent "parametro de lambda")

-- | Reconhece uma cadeia associativa a esquerda: proximo { op proximo }.
chainLeft :: Parser Expr -> [(Lexeme, BinOp)] -> Parser Expr
chainLeft next ops = next >>= go
  where
    go acc = do
      mop <- peekOp ops
      case mop of
        Nothing -> pure acc
        Just (t, op) -> do
          _ <- advance
          right <- next
          go (EBin (tokPos t) op acc right)

-- | Se o proximo token for um dos operadores da tabela, devolve-o
-- junto com o operador correspondente, sem consumir.
peekOp :: [(Lexeme, BinOp)] -> Parser (Maybe (Token, BinOp))
peekOp table = do
  t <- peek
  pure (fmap ((,) t) (lookup (tokLexeme t) table))

relOps :: [(Lexeme, BinOp)]
relOps =
  [ (TEq, Eq), (TNeq, Neq), (TLt, Lt)
  , (TLe, Le), (TGt, Gt), (TGe, Ge)
  ]

mulOps :: [(Lexeme, BinOp)]
mulOps =
  [ (TStar, Mul), (TSlash, Div)
  , (TDoubleSlash, FloorDiv), (TPercent, Mod)
  ]
