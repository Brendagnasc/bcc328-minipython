{
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- Os avisos acima sao suprimidos porque este arquivo e processado
-- pelo gerador Happy, cujo codigo gerado nao atende a -Wall.

-- | Analisador sintatico LALR gerado pelo Happy.
--
-- Diferente da GLC proposta em docs/gramatica.md, as producoes de
-- expressao nao sao estratificadas: a precedencia e a associatividade
-- sao declaradas pelas diretivas %left, %right e %nonassoc, conforme
-- exigido na secao 5 do enunciado. A linguagem reconhecida e a mesma,
-- e a AST produzida e identica a dos demais analisadores.
module MiniPython.Frontend.Parser.Happy.Grammar
  ( parseProgramLALR
  ) where

import MiniPython.Frontend.Lexer.Token
import MiniPython.Frontend.Syntax.MiniPythonSyntax
}

%name pProgram Programa
%tokentype { Token }
%error     { parseError }
%monad     { Either String }

%token
  int_lit   { Token (TIntLit _) _ }
  float_lit { Token (TFloatLit _) _ }
  str_lit   { Token (TStringLit _) _ }
  bool_lit  { Token (TBoolLit _) _ }
  ident     { Token (TIdent _) _ }

  'and'      { Token TAnd _ }
  'break'    { Token TBreak _ }
  'bool'     { Token TBool _ }
  'class'    { Token TClass _ }
  'continue' { Token TContinue _ }
  'def'      { Token TDef _ }
  'elif'     { Token TElif _ }
  'else'     { Token TElse _ }
  'float'    { Token TFloat _ }
  'for'      { Token TFor _ }
  'if'       { Token TIf _ }
  'in'       { Token TIn _ }
  'int'      { Token TInt _ }
  'lambda'   { Token TLambda _ }
  'None'     { Token TNone _ }
  'not'      { Token TNot _ }
  'or'       { Token TOr _ }
  'pass'     { Token TPass _ }
  'return'   { Token TReturn _ }
  'self'     { Token TSelf _ }
  'str'      { Token TStr _ }
  'while'    { Token TWhile _ }

  '+'   { Token TPlus _ }
  '-'   { Token TMinus _ }
  '*'   { Token TStar _ }
  '/'   { Token TSlash _ }
  '//'  { Token TDoubleSlash _ }
  '%'   { Token TPercent _ }
  '**'  { Token TDoubleStar _ }
  '=='  { Token TEq _ }
  '!='  { Token TNeq _ }
  '<'   { Token TLt _ }
  '<='  { Token TLe _ }
  '>'   { Token TGt _ }
  '>='  { Token TGe _ }
  '='   { Token TAssign _ }
  '+='  { Token TPlusAssign _ }
  '-='  { Token TMinusAssign _ }
  '*='  { Token TStarAssign _ }
  '/='  { Token TSlashAssign _ }
  '//=' { Token TDoubleSlashAssign _ }
  '%='  { Token TPercentAssign _ }
  '('   { Token TLParen _ }
  ')'   { Token TRParen _ }
  '['   { Token TLBracket _ }
  ']'   { Token TRBracket _ }
  ','   { Token TComma _ }
  ':'   { Token TColon _ }
  '.'   { Token TDot _ }
  '->'  { Token TArrow _ }

  NEWLINE { Token TNewline _ }
  INDENT  { Token TIndent _ }
  DEDENT  { Token TDedent _ }
  EOF     { Token TEOF _ }

-- Precedencia da menor para a maior, conforme a secao 6.3 da
-- especificacao. A diretiva %nonassoc nos operadores relacionais
-- torna a < b < c um erro de sintaxe.
-- 'lambda' recebe a menor precedencia de todas para resolver o
-- conflito shift/reduce da regra do corpo da lambda: ao ver um
-- operador apos o corpo, o parser desloca em vez de reduzir, de modo
-- que lambda x: x + 1 seja a funcao que soma 1, e nao a lambda
-- somada a 1. O corpo e uma expressao unica (secao 6.11).
%nonassoc LAMBDA
%nonassoc 'if' 'else'
%left 'or'
%left 'and'
%right 'not'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '//' '%'
%right NEG
%right '**'
%left '.' '[' '('

%%

Programa :: { Program }
  : Decls EOF                       { Program (reverse $1) }

Decls :: { [Decl] }
  : {- vazio -}                     { [] }
  | Decls Decl                      { $2 : $1 }

Decl :: { Decl }
  : DefFuncao                       { DFun $1 }
  | DefClasse                       { DClass $1 }
  | Comando                         { DStmt $1 }

DefFuncao :: { FunDef }
  : 'def' ident '(' Params ')' Retorno ':' Bloco
      { FunDef (tokPos $1) (identName $2) (reverse $4) $6 $8 }

Retorno :: { Maybe Type }
  : {- vazio -}                     { Nothing }
  | '->' Tipo                       { Just $2 }

Params :: { [Param] }
  : {- vazio -}                     { [] }
  | ParamList                       { $1 }

ParamList :: { [Param] }
  : Param                           { [$1] }
  | ParamList ',' Param             { $3 : $1 }

Param :: { Param }
  : 'self'                          { Param (tokPos $1) "self" Nothing }
  | ident                           { Param (tokPos $1) (identName $1) Nothing }
  | ident ':' Tipo                  { Param (tokPos $1) (identName $1) (Just $3) }

DefClasse :: { ClassDef }
  : 'class' ident Super ':' NEWLINE INDENT Campos Metodos DEDENT
      { ClassDef (tokPos $1) (identName $2) $3 (reverse $7) (reverse $8) }

Super :: { Maybe Name }
  : {- vazio -}                     { Nothing }
  | '(' ident ')'                   { Just (identName $2) }

Campos :: { [Field] }
  : {- vazio -}                     { [] }
  | Campos Campo                    { $2 : $1 }

Campo :: { Field }
  : ident ':' Tipo NEWLINE          { Field (tokPos $1) (identName $1) $3 }

Metodos :: { [FunDef] }
  : {- vazio -}                     { [] }
  | Metodos DefFuncao               { $2 : $1 }

Bloco :: { Block }
  : NEWLINE INDENT Comandos DEDENT  { reverse $3 }

Comandos :: { [Stmt] }
  : Comando                         { [$1] }
  | Comandos Comando                { $2 : $1 }

Comando :: { Stmt }
  : ComandoSimples NEWLINE          { $1 }
  | ComandoIf                       { $1 }
  | 'while' Expr ':' Bloco          { SWhile (tokPos $1) $2 $4 }
  | 'for' ident 'in' Expr ':' Bloco { SFor (tokPos $1) (identName $2) $4 $6 }

ComandoIf :: { Stmt }
  : 'if' Expr ':' Bloco Elifs Else  { SIf (tokPos $1) $2 $4 (reverse $5) $6 }

Elifs :: { [(Pos, Expr, Block)] }
  : {- vazio -}                     { [] }
  | Elifs 'elif' Expr ':' Bloco     { (tokPos $2, $3, $5) : $1 }

Else :: { Block }
  : {- vazio -}                     { [] }
  | 'else' ':' Bloco                { $3 }

ComandoSimples :: { Stmt }
  : 'pass'                          { SPass (tokPos $1) }
  | 'break'                         { SBreak (tokPos $1) }
  | 'continue'                      { SContinue (tokPos $1) }
  | 'return'                        { SReturn (tokPos $1) Nothing }
  | 'return' Expr                   { SReturn (tokPos $1) (Just $2) }
  | ident ':' Tipo '=' Expr         { SVarDecl (tokPos $1) (identName $1) $3 $5 }
  | Expr '=' Expr                   {% mkAssign $1 $3 }
  | Expr OpAumentado Expr           {% mkAugAssign $1 $2 $3 }
  | Expr                            { SExpr (exprPos $1) $1 }

OpAumentado :: { AugOp }
  : '+='                            { AddAssign }
  | '-='                            { SubAssign }
  | '*='                            { MulAssign }
  | '/='                            { DivAssign }
  | '//='                           { FloorDivAssign }
  | '%='                            { ModAssign }

Tipo :: { Type }
  : 'int'                           { TyInt (tokPos $1) }
  | 'float'                         { TyFloat (tokPos $1) }
  | 'bool'                          { TyBool (tokPos $1) }
  | 'str'                           { TyStr (tokPos $1) }
  | 'None'                          { TyNone (tokPos $1) }
  | ident '[' Tipo ']'              { TyList (tokPos $1) $3 }
  | ident                           { mkNameType $1 }
  | '(' Tipos ')' '->' Tipo         { TyFun (tokPos $1) (reverse $2) $5 }

Tipos :: { [Type] }
  : {- vazio -}                     { [] }
  | TipoList                        { $1 }

TipoList :: { [Type] }
  : Tipo                            { [$1] }
  | TipoList ',' Tipo               { $3 : $1 }

Expr :: { Expr }
  : Expr 'if' Expr 'else' Expr      { ECond (exprPos $1) $1 $3 $5 }
  | Expr 'or' Expr                  { EBin (exprPos $1) Or $1 $3 }
  | Expr 'and' Expr                 { EBin (exprPos $1) And $1 $3 }
  | 'not' Expr                      { EUn (tokPos $1) Not $2 }
  | Expr '==' Expr                  { EBin (exprPos $1) Eq $1 $3 }
  | Expr '!=' Expr                  { EBin (exprPos $1) Neq $1 $3 }
  | Expr '<' Expr                   { EBin (exprPos $1) Lt $1 $3 }
  | Expr '<=' Expr                  { EBin (exprPos $1) Le $1 $3 }
  | Expr '>' Expr                   { EBin (exprPos $1) Gt $1 $3 }
  | Expr '>=' Expr                  { EBin (exprPos $1) Ge $1 $3 }
  | Expr '+' Expr                   { EBin (exprPos $1) Add $1 $3 }
  | Expr '-' Expr                   { EBin (exprPos $1) Sub $1 $3 }
  | Expr '*' Expr                   { EBin (exprPos $1) Mul $1 $3 }
  | Expr '/' Expr                   { EBin (exprPos $1) Div $1 $3 }
  | Expr '//' Expr                  { EBin (exprPos $1) FloorDiv $1 $3 }
  | Expr '%' Expr                   { EBin (exprPos $1) Mod $1 $3 }
  | '-' Expr %prec NEG              { EUn (tokPos $1) Neg $2 }
  | Expr '**' Expr                  { EBin (exprPos $1) Pow $1 $3 }
  | 'lambda' LambdaParams ':' Expr %prec LAMBDA
                                    { ELambda (tokPos $1) (reverse $2) $4 }
  | Posfixa                         { $1 }

LambdaParams :: { [Name] }
  : {- vazio -}                     { [] }
  | LambdaList                      { $1 }

LambdaList :: { [Name] }
  : ident                           { [identName $1] }
  | LambdaList ',' ident            { identName $3 : $1 }

Posfixa :: { Expr }
  : Posfixa '(' Args ')'            { ECall (exprPos $1) $1 (reverse $3) }
  | Posfixa '[' Expr ']'            { EIndex (exprPos $1) $1 $3 }
  | Posfixa '.' ident               { EField (exprPos $1) $1 (identName $3) }
  | Atomo                           { $1 }

Args :: { [Expr] }
  : {- vazio -}                     { [] }
  | ArgList                         { $1 }

ArgList :: { [Expr] }
  : Expr                            { [$1] }
  | ArgList ',' Expr                { $3 : $1 }

Atomo :: { Expr }
  : int_lit                         { mkInt $1 }
  | float_lit                       { mkFloat $1 }
  | str_lit                         { mkStr $1 }
  | bool_lit                        { mkBool $1 }
  | 'None'                          { ENone (tokPos $1) }
  | 'self'                          { ESelf (tokPos $1) }
  | ident                           { EVar (tokPos $1) (identName $1) }
  | 'int'                           { EVar (tokPos $1) "int" }
  | 'float'                         { EVar (tokPos $1) "float" }
  | 'str'                           { EVar (tokPos $1) "str" }
  | 'bool'                          { EVar (tokPos $1) "bool" }
  | '[' Args ']'                    { EList (tokPos $1) (reverse $2) }
  | '(' Expr ')'                    { $2 }

{
-- | Reconhece um programa completo a partir da lista de tokens.
parseProgramLALR :: [Token] -> Either String Program
parseProgramLALR = pProgram

parseError :: [Token] -> Either String a
parseError [] = Left "erro sintatico: fim inesperado da entrada"
parseError (t : _) =
  Left ( show (posLine (tokPos t)) ++ ":" ++ show (posCol (tokPos t))
       ++ ": erro sintatico: token inesperado "
       ++ describeLexeme (tokLexeme t) )

identName :: Token -> Name
identName t = case tokLexeme t of
  TIdent n -> n
  _        -> error "identName: token nao e identificador"

mkInt :: Token -> Expr
mkInt t = case tokLexeme t of
  TIntLit n -> EInt (tokPos t) n
  _         -> error "mkInt"

mkFloat :: Token -> Expr
mkFloat t = case tokLexeme t of
  TFloatLit d -> EFloat (tokPos t) d
  _           -> error "mkFloat"

mkStr :: Token -> Expr
mkStr t = case tokLexeme t of
  TStringLit s -> EStr (tokPos t) s
  _            -> error "mkStr"

mkBool :: Token -> Expr
mkBool t = case tokLexeme t of
  TBoolLit b -> EBool (tokPos t) b
  _          -> error "mkBool"

-- | Identificadores iniciados por maiuscula sao tipos de classe; os
-- demais sao variaveis de tipo (secao 3.5 da especificacao).
mkNameType :: Token -> Type
mkNameType t = case tokLexeme t of
  TIdent n
    | not (null n) && head n `elem` ['A' .. 'Z'] -> TyClass (tokPos t) n
    | otherwise -> TyVar (tokPos t) n
  _ -> error "mkNameType"

-- | Converte a expressao a esquerda do '=' em alvo de atribuicao.
-- Mesma estrategia dos demais analisadores: a producao aceita uma
-- expressao qualquer e a restricao a alvos validos e aplicada na
-- acao semantica.
mkAssign :: Expr -> Expr -> Either String Stmt
mkAssign alvo val = do
  lv <- toLValue alvo
  pure (SAssign (exprPos alvo) lv val)

mkAugAssign :: Expr -> AugOp -> Expr -> Either String Stmt
mkAugAssign alvo op val = do
  lv <- toLValue alvo
  pure (SAugAssign (exprPos alvo) lv op val)

toLValue :: Expr -> Either String LValue
toLValue e = case e of
  EVar p n     -> Right (LVar p n)
  EIndex p b i -> Right (LIndex p b i)
  EField p b n -> Right (LField p b n)
  _ -> Left ( show (posLine (exprPos e)) ++ ":" ++ show (posCol (exprPos e))
            ++ ": erro sintatico: alvo de atribuicao invalido" )
}
