{-# OPTIONS_GHC -Wno-orphans #-}
-- As instancias de Pretty sao declaradas aqui, e nao junto aos tipos,
-- para manter a AST livre de dependencias de apresentacao. Essa e a
-- mesma organizacao usada nos exemplos da disciplina.
-- | Impressao da AST do MiniPython como codigo fonte reconstituido.
--
-- A impressao de expressoes e estratificada por precedencia: cada nivel
-- trata seus operadores e delega o restante ao nivel seguinte, inserindo
-- parenteses apenas onde sao necessarios. Nenhuma instancia de Show
-- gerada pelo compilador e utilizada.
module MiniPython.Frontend.Pretty.MiniPythonPretty
  ( module MiniPython.Utils.Pretty
  ) where

import MiniPython.Frontend.Syntax.MiniPythonSyntax
import MiniPython.Utils.Pretty
import Prelude hiding ((<>))

-- Largura da indentacao na saida, em espacos.
indentWidth :: Int
indentWidth = 4

instance Pretty Program where
  ppr (Program ds) = vcat (map ppr ds)

instance Pretty Decl where
  ppr (DFun fd)   = ppr fd
  ppr (DClass cd) = ppr cd
  ppr (DStmt s)   = ppr s

instance Pretty FunDef where
  ppr fd =
    (text "def" <+> text (fnName fd)
      <> parens (commaSep (map ppr (fnParams fd)))
      <> pprReturn (fnReturn fd) <> colon)
    $$ pprBlock (fnBody fd)
    where
      pprReturn Nothing  = empty
      pprReturn (Just t) = space <> text "->" <+> ppr t

instance Pretty Param where
  ppr p = case paramType p of
    Nothing -> text (paramName p)
    Just t  -> text (paramName p) <> colon <+> ppr t

instance Pretty ClassDef where
  ppr cd =
    (text "class" <+> text (clsName cd) <> super <> colon)
    $$ nest indentWidth body
    where
      super = case clsSuper cd of
        Just s  -> parens (text s)
        Nothing -> empty
      body
        | null (clsFields cd) && null (clsMethods cd) = text "pass"
        | otherwise =
            vcat (map ppr (clsFields cd))
              $$ vcat (map ppr (clsMethods cd))

instance Pretty Field where
  ppr f = text (fieldName f) <> colon <+> ppr (fieldType f)

instance Pretty Stmt where
  ppr (SVarDecl _ n t e) =
    text n <> colon <+> ppr t <+> equals <+> ppr e
  ppr (SAssign _ lv e) =
    ppr lv <+> equals <+> ppr e
  ppr (SAugAssign _ lv op e) =
    ppr lv <+> text (augOpText op) <+> ppr e
  ppr (SIf _ c thenB elifs elseB) =
    (text "if" <+> ppr c <> colon)
      $$ pprBlock thenB
      $$ vcat (map pprElif elifs)
      $$ pprElse elseB
    where
      pprElif (_, ec, eb) = (text "elif" <+> ppr ec <> colon) $$ pprBlock eb
      pprElse [] = empty
      pprElse b  = (text "else" <> colon) $$ pprBlock b
  ppr (SWhile _ c body) =
    (text "while" <+> ppr c <> colon) $$ pprBlock body
  ppr (SFor _ v it body) =
    (text "for" <+> text v <+> text "in" <+> ppr it <> colon) $$ pprBlock body
  ppr (SReturn _ Nothing)  = text "return"
  ppr (SReturn _ (Just e)) = text "return" <+> ppr e
  ppr (SBreak _)    = text "break"
  ppr (SContinue _) = text "continue"
  ppr (SPass _)     = text "pass"
  ppr (SExpr _ e)   = ppr e

instance Pretty LValue where
  ppr (LVar _ n)     = text n
  ppr (LIndex _ e i) = pprAtom e <> brackets (ppr i)
  ppr (LField _ e n) = pprAtom e <> char '.' <> text n

instance Pretty Type where
  ppr (TyInt _)        = text "int"
  ppr (TyFloat _)      = text "float"
  ppr (TyBool _)       = text "bool"
  ppr (TyStr _)        = text "str"
  ppr (TyNone _)       = text "None"
  ppr (TyList _ t)     = text "list" <> brackets (ppr t)
  ppr (TyFun _ ps r)   = parens (commaSep (map ppr ps)) <+> text "->" <+> ppr r
  ppr (TyClass _ n)    = text n
  ppr (TyVar _ n)      = text n

-- Expressoes, por nivel de precedencia (secoes 6.3 a 6.5 e 6.10).
-- Do menos para o mais firme: condicional, or, and, not, relacionais,
-- aditivos, multiplicativos, unario, potencia, posfixos, atomos.
instance Pretty Expr where
  ppr = pprCond

pprCond :: Expr -> Doc
pprCond (ECond _ t c f) =
  hsep [pprOr t, text "if", pprOr c, text "else", pprCond f]
pprCond e = pprOr e

pprOr :: Expr -> Doc
pprOr (EBin _ Or a b) = hsep [pprOr a, text "or", pprAnd b]
pprOr e = pprAnd e

pprAnd :: Expr -> Doc
pprAnd (EBin _ And a b) = hsep [pprAnd a, text "and", pprNot b]
pprAnd e = pprNot e

pprNot :: Expr -> Doc
pprNot (EUn _ Not e) = text "not" <+> pprNot e
pprNot e = pprRel e

pprRel :: Expr -> Doc
pprRel (EBin _ op a b)
  | isRel op = hsep [pprAdd a, text (binOpText op), pprAdd b]
pprRel e = pprAdd e

pprAdd :: Expr -> Doc
pprAdd (EBin _ op a b)
  | op == Add || op == Sub = hsep [pprAdd a, text (binOpText op), pprMul b]
pprAdd e = pprMul e

pprMul :: Expr -> Doc
pprMul (EBin _ op a b)
  | isMul op = hsep [pprMul a, text (binOpText op), pprUnary b]
pprMul e = pprUnary e

pprUnary :: Expr -> Doc
pprUnary (EUn _ Neg e) = char '-' <> pprUnary e
pprUnary e = pprPow e

-- Potencia associa a direita e liga mais forte que o menos unario.
pprPow :: Expr -> Doc
pprPow (EBin _ Pow a b) = pprPostfix a <> text "**" <> pprPow b
pprPow e = pprPostfix e

pprPostfix :: Expr -> Doc
pprPostfix (ECall _ f as)  = pprPostfix f <> parens (commaSep (map ppr as))
pprPostfix (EIndex _ e i)  = pprPostfix e <> brackets (ppr i)
pprPostfix (EField _ e n)  = pprPostfix e <> char '.' <> text n
pprPostfix e = pprAtom e

pprAtom :: Expr -> Doc
pprAtom (EInt _ n)   = integer n
pprAtom (EFloat _ d) = double d
pprAtom (EStr _ s)   = text (show s)
pprAtom (EBool _ b)  = text (if b then "True" else "False")
pprAtom (ENone _)    = text "None"
pprAtom (EVar _ n)   = text n
pprAtom (ESelf _)    = text "self"
pprAtom (EList _ es) = brackets (commaSep (map ppr es))
pprAtom (ELambda _ ps body) =
  parens (text "lambda" <> params <> colon <+> ppr body)
  where
    params
      | null ps   = empty
      | otherwise = space <> commaSep (map text ps)
-- Qualquer construcao de precedencia menor alcancada aqui precisa de
-- parenteses para preservar a estrutura da arvore.
pprAtom e = parens (ppr e)

-- | Um bloco indentado. Blocos vazios nao ocorrem na AST, mas a
-- impressao e total para que o compilador nao emita aviso.
pprBlock :: Block -> Doc
pprBlock [] = nest indentWidth (text "pass")
pprBlock ss = nest indentWidth (vcat (map ppr ss))

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate (comma <> space)

isRel :: BinOp -> Bool
isRel op = op `elem` [Eq, Neq, Lt, Le, Gt, Ge]

isMul :: BinOp -> Bool
isMul op = op `elem` [Mul, Div, FloorDiv, Mod]

binOpText :: BinOp -> String
binOpText op = case op of
  Add -> "+" ; Sub -> "-" ; Mul -> "*" ; Div -> "/"
  FloorDiv -> "//" ; Mod -> "%" ; Pow -> "**"
  Eq -> "==" ; Neq -> "!=" ; Lt -> "<" ; Le -> "<=" ; Gt -> ">" ; Ge -> ">="
  And -> "and" ; Or -> "or"

augOpText :: AugOp -> String
augOpText op = case op of
  AddAssign -> "+=" ; SubAssign -> "-=" ; MulAssign -> "*="
  DivAssign -> "/=" ; FloorDivAssign -> "//=" ; ModAssign -> "%="
