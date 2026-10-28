-- | Impressao da AST do MiniPython como arvore ASCII.
--
-- A AST e convertida em um Tree String e desenhada com drawTree, de
-- Data.Tree, como pede a secao 3 do enunciado. O rotulo de cada no e
-- o construtor seguido dos seus campos atomicos; os filhos sao os
-- subnos. Anotacoes de tipo entram no rotulo, escritas como no fonte.
--
-- Os blocos de if, while e for ficam sob um no auxiliar (then, elif,
-- else, body), em minusculas para nao se confundirem com construtores,
-- porque sem ele nao daria para saber onde termina a condicao e onde
-- comeca o corpo. Nenhuma instancia de Show derivada e usada.
module MiniPython.Frontend.Pretty.MiniPythonPretty
  ( toTree
  , showType
  ) where

import Data.List (intercalate)
import Data.Tree (Tree (..))
import MiniPython.Frontend.Syntax.MiniPythonSyntax

toTree :: Program -> Tree String
toTree (Program ds) = Node "Program" (map declTree ds)

declTree :: Decl -> Tree String
declTree (DFun fd)   = funTree fd
declTree (DClass cd) = classTree cd
declTree (DStmt s)   = stmtTree s

-- | A assinatura vai inteira no rotulo e os filhos sao o corpo.
funTree :: FunDef -> Tree String
funTree fd = Node ("FunDef " ++ fnName fd ++ signature) (map stmtTree (fnBody fd))
  where
    signature =
      "(" ++ intercalate ", " (map param (fnParams fd)) ++ ")"
        ++ maybe "" ((" -> " ++) . showType) (fnReturn fd)
    param p = paramName p ++ maybe "" ((": " ++) . showType) (paramType p)

classTree :: ClassDef -> Tree String
classTree cd =
  Node ("ClassDef " ++ clsName cd ++ maybe "" (\s -> "(" ++ s ++ ")") (clsSuper cd))
       (map fieldTree (clsFields cd) ++ map funTree (clsMethods cd))
  where
    fieldTree f = leaf ("Field " ++ fieldName f ++ ": " ++ showType (fieldType f))

stmtTree :: Stmt -> Tree String
stmtTree s = case s of
  SVarDecl _ n t e ->
    Node ("SVarDecl " ++ n ++ ": " ++ showType t) [exprTree e]
  SAssign _ lv e ->
    Node "SAssign" [lvalueTree lv, exprTree e]
  SAugAssign _ lv op e ->
    Node ("SAugAssign " ++ augOpName op) [lvalueTree lv, exprTree e]
  SIf _ c thenB elifs elseB ->
    Node "SIf" ([exprTree c, block "then" thenB] ++ map elifTree elifs ++ elseTree elseB)
  SWhile _ c body ->
    Node "SWhile" [exprTree c, block "body" body]
  SFor _ v it body ->
    Node ("SFor " ++ v) [exprTree it, block "body" body]
  SReturn _ Nothing  -> leaf "SReturn"
  SReturn _ (Just e) -> Node "SReturn" [exprTree e]
  SBreak _    -> leaf "SBreak"
  SContinue _ -> leaf "SContinue"
  SPass _     -> leaf "SPass"
  SExpr _ e   -> Node "SExpr" [exprTree e]
  where
    elifTree (_, c, b) = Node "elif" [exprTree c, block "then" b]
    elseTree [] = []
    elseTree b  = [block "else" b]

block :: String -> Block -> Tree String
block name ss = Node name (map stmtTree ss)

lvalueTree :: LValue -> Tree String
lvalueTree lv = case lv of
  LVar _ n     -> leaf ("LVar " ++ n)
  LIndex _ e i -> Node "LIndex" [exprTree e, exprTree i]
  LField _ e n -> Node ("LField " ++ n) [exprTree e]

exprTree :: Expr -> Tree String
exprTree e = case e of
  EInt _ n    -> leaf ("EInt " ++ show n)
  EFloat _ d  -> leaf ("EFloat " ++ show d)
  EStr _ str  -> leaf ("EStr " ++ show str)
  EBool _ b   -> leaf (if b then "EBool True" else "EBool False")
  ENone _     -> leaf "ENone"
  EVar _ n    -> leaf ("EVar " ++ n)
  ESelf _     -> leaf "ESelf"
  EBinOp _ op a b -> Node ("EBinOp " ++ binOpName op) [exprTree a, exprTree b]
  EUnOp _ op a    -> Node ("EUnOp " ++ unOpName op) [exprTree a]
  -- O primeiro filho e a funcao chamada; os demais, os argumentos.
  ECall _ f args  -> Node "ECall" (exprTree f : map exprTree args)
  EIndex _ a i    -> Node "EIndex" [exprTree a, exprTree i]
  EField _ a n    -> Node ("EField " ++ n) [exprTree a]
  EList _ es      -> Node "EList" (map exprTree es)
  ELambda _ ps b  -> Node (unwords ("ELambda" : ps)) [exprTree b]
  -- Filhos na ordem do fonte: valor, condicao, alternativa.
  ECond _ t c f   -> Node "ECond" [exprTree t, exprTree c, exprTree f]

leaf :: String -> Tree String
leaf l = Node l []

-- | Tipo escrito como no fonte, para o rotulo dos nos.
showType :: Type -> String
showType t = case t of
  TyInt _       -> "int"
  TyFloat _     -> "float"
  TyBool _      -> "bool"
  TyStr _       -> "str"
  TyNone _      -> "None"
  TyList _ a    -> "list[" ++ showType a ++ "]"
  TyFun _ ps r  -> "(" ++ intercalate ", " (map showType ps) ++ ") -> " ++ showType r
  TyClass _ n   -> n
  TyVar _ n     -> n

binOpName :: BinOp -> String
binOpName op = case op of
  Add -> "Add" ; Sub -> "Sub" ; Mul -> "Mul" ; Div -> "Div"
  FloorDiv -> "FloorDiv" ; Mod -> "Mod" ; Pow -> "Pow"
  Eq -> "Eq" ; Neq -> "Neq" ; Lt -> "Lt" ; Le -> "Le" ; Gt -> "Gt" ; Ge -> "Ge"
  And -> "And" ; Or -> "Or"

unOpName :: UnOp -> String
unOpName Neg = "Neg"
unOpName Not = "Not"

augOpName :: AugOp -> String
augOpName op = case op of
  AddAssign -> "AddAssign" ; SubAssign -> "SubAssign" ; MulAssign -> "MulAssign"
  DivAssign -> "DivAssign" ; FloorDivAssign -> "FloorDivAssign" ; ModAssign -> "ModAssign"
