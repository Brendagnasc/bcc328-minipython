-- | Valores do MiniPython em tempo de execucao (secao 3.1 do enunciado).
module MiniPython.Interpreter.Value
  ( Value (..)
  , Object (..)
  , Scope
  , showValue
  , showFloat
  , parseInt
  , parseFloat
  ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.Foldable (toList)
import Data.IORef (IORef, readIORef)
import Data.List (dropWhileEnd, intercalate, isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Unique (Unique)
import MiniPython.Frontend.Syntax.MiniPythonSyntax (Expr, FunDef (..), Name)
import Numeric (floatToDigits)

-- | Escopo: cada nome aponta para uma referencia mutavel, porque toda
-- variavel pode ser reatribuida (regra 4 da secao 10).
type Scope = IORef (Map.Map Name (IORef Value))

data Value
  = VInt Integer
  | VFloat Double
  | VBool Bool
  | VStr String
  | VNone
  -- | Lista mutavel: xs[i] = e e append alteram a mesma lista.
  | VList (IORef (Seq Value))
  | VObject Object
  -- | Funcao declarada com def e os escopos que o corpo enxerga alem
  -- dos parametros: so o global, e a propria funcao se for local.
  | VFun FunDef [Scope]
  -- | Lambda: parametros, corpo e os escopos capturados na criacao.
  | VLambda [Name] Expr [Scope]
  -- | Funcao com os primeiros argumentos ja dados. E assim que fica um
  -- metodo ligado ao objeto (o self) e o xs.append de uma lista.
  | VPartial Value [Value]
  -- | Nome de classe: chamado, cria um objeto.
  | VClass Name
  | VBuiltin Name

-- | Objeto: a classe concreta, que decide o despacho dos metodos, e os
-- campos, todos criados junto com o objeto.
data Object = Object
  { objId     :: Unique
  , objClass  :: Name
  , objFields :: Map.Map Name (IORef Value)
  }

-- | Texto do valor como em str() do Python.
showValue :: Value -> IO String
showValue v = case v of
  VInt n     -> pure (show n)
  VFloat d   -> pure (showFloat d)
  VBool b    -> pure (if b then "True" else "False")
  VStr s     -> pure s
  VNone      -> pure "None"
  VList ref  -> do
    xs <- readIORef ref
    ss <- mapM repr (toList xs)
    pure ("[" ++ intercalate ", " ss ++ "]")
  VObject o  -> pure ("<" ++ objClass o ++ " object>")
  VFun fd _  -> pure ("<function " ++ fnName fd ++ ">")
  VLambda {} -> pure "<function <lambda>>"
  VPartial f _ -> showValue f
  VClass c   -> pure ("<class '" ++ c ++ "'>")
  VBuiltin n -> pure ("<built-in function " ++ n ++ ">")
  where
    repr (VStr s) = pure ("'" ++ s ++ "'")
    repr x        = showValue x

-- | Float como no repr do Python 3: menor quantidade de digitos que
-- identifica o numero, notacao fixa entre 1e-4 e 1e16 e expoente com
-- sinal e pelo menos dois digitos fora dessa faixa (1e+16, 1e-05).
showFloat :: Double -> String
showFloat d
  | isNaN d = "nan"
  | isInfinite d = if d > 0 then "inf" else "-inf"
  | d < 0 || isNegativeZero d = '-' : showFloat (negate d)
  | d == 0 = "0.0"
  | e > -4 && e <= 16 = fixa
  | otherwise = cientifica
  where
    (ds, e) = floatToDigits 10 d
    digitos = concatMap show ds
    n = length digitos
    fixa
      | e <= 0    = "0." ++ replicate (negate e) '0' ++ digitos
      | e >= n    = digitos ++ replicate (e - n) '0' ++ ".0"
      | otherwise = take e digitos ++ "." ++ drop e digitos
    cientifica =
      take 1 digitos ++ (if n > 1 then '.' : drop 1 digitos else "")
        ++ "e" ++ (if e - 1 < 0 then "-" else "+") ++ doisDigitos (abs (e - 1))
    doisDigitos x = let s = show x in if length s < 2 then '0' : s else s

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

-- | int(s) do Python: espacos nas pontas, sinal e _ entre digitos.
parseInt :: String -> Maybe Integer
parseInt s0 = case strip s0 of
  '-' : r -> negate <$> digitos r
  '+' : r -> digitos r
  r       -> digitos r
  where
    digitos r
      | valido r  = Just (read (filter (/= '_') r))
      | otherwise = Nothing
    valido r = case (r, reverse r) of
      (c : _, u : _) -> isDigit c && isDigit u && all (\x -> isDigit x || x == '_') r
                        && not ("__" `isInfixOf` r)
      _ -> False

-- | float(s) do Python: aceita 3, 3.5, .5, 5., 1e10, inf e nan.
parseFloat :: String -> Maybe Double
parseFloat s0 = case strip s0 of
  '-' : r -> negate <$> semSinal r
  '+' : r -> semSinal r
  r       -> semSinal r
  where
    semSinal r
      | map toLower r `elem` ["inf", "infinity"] = Just (1 / 0)
      | map toLower r == "nan" = Just (0 / 0)
      | otherwise =
          let (inteira, r1) = span isDigit r
              (frac, r2) = case r1 of
                '.' : t -> span isDigit t
                _       -> ("", r1)
              expoente = case r2 of
                c : t | c `elem` "eE" -> case t of
                  '-' : u -> fmap ('-' :) (soDigitos u)
                  '+' : u -> soDigitos u
                  u       -> soDigitos u
                "" -> Just "0"
                _  -> Nothing
          in if null inteira && null frac
               then Nothing
               else do
                 x <- expoente
                 pure (read (ou "0" inteira ++ "." ++ ou "0" frac ++ "e" ++ x))
    soDigitos u = if not (null u) && all isDigit u then Just u else Nothing
    ou padrao t = if null t then padrao else t
