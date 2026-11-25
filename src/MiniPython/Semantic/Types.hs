-- | Tipos usados pelo verificador.
--
-- Diferente de Type, da AST, aqui nao ha posicao e ha dois tipos de
-- variavel: TGen e a variavel de tipo escrita nas anotacoes (a, b, ...)
-- e TMeta e uma incognita criada pela inferencia, resolvida por
-- unificacao. Dentro do corpo de uma funcao polimorfica as TGen sao
-- rigidas; elas so viram incognitas quando a funcao e instanciada numa
-- chamada.
module MiniPython.Semantic.Types
  ( Ty (..)
  , Scheme (..)
  , metas
  , gens
  , substGen
  , showTy
  , showTys
  ) where

import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import MiniPython.Frontend.Syntax.MiniPythonSyntax (Name)

data Ty
  = TInt
  | TFloat
  | TBool
  | TStr
  | TNone
  | TList Ty
  | TFun [Ty] Ty
  | TClass Name
  | TGen Name
  | TMeta Int
  deriving (Eq, Ord)

-- | Tipo de uma funcao declarada com def: as variaveis listadas sao
-- quantificadas e trocadas por incognitas novas a cada uso.
data Scheme = Forall [Name] Ty

-- | Incognitas que aparecem no tipo, na ordem em que aparecem.
metas :: Ty -> [Int]
metas t = nub (go t)
  where
    go ty = case ty of
      TMeta i   -> [i]
      TList a   -> go a
      TFun ps r -> concatMap go ps ++ go r
      _         -> []

-- | Variaveis de tipo das anotacoes, na ordem em que aparecem.
gens :: Ty -> [Name]
gens t = nub (go t)
  where
    go ty = case ty of
      TGen n    -> [n]
      TList a   -> go a
      TFun ps r -> concatMap go ps ++ go r
      _         -> []

substGen :: Map.Map Name Ty -> Ty -> Ty
substGen s ty = case ty of
  TGen n    -> Map.findWithDefault ty n s
  TList a   -> TList (substGen s a)
  TFun ps r -> TFun (map (substGen s) ps) (substGen s r)
  _         -> ty

-- | Tipo escrito como nas anotacoes do fonte.
showTy :: Ty -> String
showTy ty = case ty of
  TInt      -> "int"
  TFloat    -> "float"
  TBool     -> "bool"
  TStr      -> "str"
  TNone     -> "None"
  TList a   -> "list[" ++ showTy a ++ "]"
  TFun ps r -> "(" ++ intercalate ", " (map showTy ps) ++ ") -> " ++ showTy r
  TClass n  -> n
  TGen n    -> n
  TMeta i   -> "?" ++ show i

-- | Escreve os tipos de uma mesma mensagem renumerando as incognitas a
-- partir de ?0, na ordem em que aparecem. Assim a mensagem nao depende
-- de quantas incognitas o verificador ja criou antes.
showTys :: [Ty] -> [String]
showTys ts = map (showTy . renum) ts
  where
    nomes = Map.fromList (zip (nub (concatMap metas ts)) [0 ..])
    renum ty = case ty of
      TMeta i   -> TMeta (Map.findWithDefault i i nomes)
      TList a   -> TList (renum a)
      TFun ps r -> TFun (map renum ps) (renum r)
      _         -> ty
