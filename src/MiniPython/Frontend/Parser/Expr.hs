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
  m <- accept TkIf
  case m of
    Nothing -> pure e
    Just _ -> do
      cond <- pOr
      _ <- expect TkElse "else" "expressao condicional"
      alt <- pExpr
      pure (ECond (exprPos e) e cond alt)

-- | ExprOu ::= ExprE { 'or' ExprE }
pOr :: Parser Expr
pOr = chainLeft pAnd [(TkOr, Or)]

-- | ExprE ::= ExprNao { 'and' ExprNao }
pAnd :: Parser Expr
pAnd = chainLeft pNot [(TkAnd, And)]

-- | ExprNao ::= 'not' ExprNao | ExprRel
pNot :: Parser Expr
pNot = do
  m <- accept TkNot
  case m of
    Just t  -> EUnOp (tokPos t) Not <$> pNot
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
    Just (_, op) -> do
      _ <- advance
      right <- pAdd
      pure (EBinOp (exprPos left) op left right)

-- | ExprAdit ::= ExprMult { ( '+' | '-' ) ExprMult }
pAdd :: Parser Expr
pAdd = chainLeft pMul [(TkPlus, Add), (TkMinus, Sub)]

-- | ExprMult ::= ExprUnaria { ( '*' | '/' | '//' | '%' ) ExprUnaria }
pMul :: Parser Expr
pMul = chainLeft pUnary mulOps

-- | ExprUnaria ::= '-' ExprUnaria | ExprPot
--
-- O menos unario liga mais forte que ** (secao 6.3 da especificacao),
-- portanto -2 ** 2 e lido como (-2) ** 2.
pUnary :: Parser Expr
pUnary = do
  m <- accept TkMinus
  case m of
    Just t  -> EUnOp (tokPos t) Neg <$> pUnary
    Nothing -> pPow

-- | ExprPot ::= ExprPosfixa [ '**' ExprPot ]
--
-- Recursao a direita no segundo operando: ** associa a direita.
pPow :: Parser Expr
pPow = do
  base <- pPostfix
  m <- accept TkDStar
  case m of
    Nothing -> pure base
    Just _  -> EBinOp (exprPos base) Pow base <$> pPow

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
        TkLParen -> do
          _ <- advance
          args <- pArgsUntil TkRParen
          _ <- expect TkRParen ")" "chamada de funcao"
          go (ECall (exprPos acc) acc args)
        TkLBracket -> do
          _ <- advance
          idx <- pExpr
          _ <- expect TkRBracket "]" "acesso indexado"
          go (EIndex (exprPos acc) acc idx)
        TkDot -> do
          _ <- advance
          (_, n) <- expectIdent "acesso a campo"
          go (EField (exprPos acc) acc n)
        _ -> pure acc

-- | Args ::= Expr { ',' Expr }
pArgs :: Parser [Expr]
pArgs = sepBy1 TkComma pExpr

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
    TkIntLit n    -> advance >> pure (EInt p n)
    TkFloatLit d  -> advance >> pure (EFloat p d)
    TkStrLit s -> advance >> pure (EStr p s)
    TkBoolLit b   -> advance >> pure (EBool p b)
    TkNoneKw        -> advance >> pure (ENone p)
    TkSelf        -> advance >> pure (ESelf p)
    TkIdent n     -> advance >> pure (EVar p n)
    -- int, float, str e bool sao palavras reservadas de tipo e tambem
    -- funcoes primitivas de conversao (secao 8 da especificacao). Em
    -- posicao de expressao valem como nome de funcao; a chamada e
    -- montada pelo nivel posfixo.
    TkIntKw         -> advance >> pure (EVar p "int")
    TkFloatKw       -> advance >> pure (EVar p "float")
    TkStrKw         -> advance >> pure (EVar p "str")
    TkBoolKw        -> advance >> pure (EVar p "bool")
    TkLBracket    -> do
      _ <- advance
      es <- pArgsUntil TkRBracket
      _ <- expect TkRBracket "]" "literal de lista"
      pure (EList p es)
    TkLambda -> do
      _ <- advance
      ps <- pLambdaParams
      _ <- expect TkColon ":" "expressao lambda"
      ELambda p ps <$> pExpr
    TkLParen -> do
      _ <- advance
      e <- pExpr
      _ <- expect TkRParen ")" "expressao entre parenteses"
      pure e
    _ -> failWith "uma expressao" "expressao"

-- | Parametros de lambda: nomes sem anotacao de tipo, possivelmente
-- nenhum, como em @lambda: 42@ (secao 6.11 da especificacao).
pLambdaParams :: Parser [Name]
pLambdaParams = do
  semParams <- check TkColon
  if semParams
    then pure []
    else map snd <$> sepBy1 TkComma (expectIdent "parametro de lambda")

-- | Reconhece uma cadeia associativa a esquerda: proximo { op proximo }.
chainLeft :: Parser Expr -> [(Lexeme, BinOp)] -> Parser Expr
chainLeft next ops = next >>= go
  where
    go acc = do
      mop <- peekOp ops
      case mop of
        Nothing -> pure acc
        Just (_, op) -> do
          _ <- advance
          right <- next
          go (EBinOp (exprPos acc) op acc right)

-- | Se o proximo token for um dos operadores da tabela, devolve-o
-- junto com o operador correspondente, sem consumir.
peekOp :: [(Lexeme, BinOp)] -> Parser (Maybe (Token, BinOp))
peekOp table = do
  t <- peek
  pure (fmap ((,) t) (lookup (tokLexeme t) table))

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
