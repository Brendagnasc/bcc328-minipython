-- | Interpretador do MiniPython: executa direto a AST de um programa ja
-- verificado pelo verificador de tipos.
--
-- O ambiente e uma lista de escopos, o mais interno primeiro e o global
-- por ultimo. Cada bloco abre um escopo novo, como no verificador. O
-- corpo de uma funcao def ve so os seus parametros e o global (def nao
-- forma closure); a lambda guarda a lista de escopos de onde foi
-- criada.
--
-- Erro de execucao (indice fora da lista, divisao por zero, entrada
-- invalida) e uma excecao com posicao, que runProgram devolve como
-- Left.
module MiniPython.Interpreter.Interp
  ( Console (..)
  , stdConsole
  , runProgram
  ) where

import Control.Exception (Exception, catch, throwIO, try)
import Control.Monad (forM, forM_, void, zipWithM)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Unique (newUnique)
import MiniPython.Frontend.Lexer.Token (Pos)
import MiniPython.Frontend.Syntax.MiniPythonSyntax
import MiniPython.Interpreter.Value
import MiniPython.Utils.Error
import System.IO.Error (isEOFError)

-- | Entrada e saida do programa interpretado: o executavel usa stdin e
-- stdout, e os testes uma lista de linhas.
data Console = Console
  { conRead  :: IO (Maybe String)   -- Nothing no fim da entrada
  , conWrite :: String -> IO ()
  }

stdConsole :: Console
stdConsole = Console ler putStrLn
  where
    ler = (Just <$> getLine) `catch` \e ->
      if isEOFError e then pure Nothing else ioError e

data RuntimeError = RuntimeError Pos String

instance Show RuntimeError where
  show (RuntimeError _ m) = m

instance Exception RuntimeError

erro :: Pos -> String -> IO a
erro p m = throwIO (RuntimeError p m)

-- | Classe em tempo de execucao: pai, campos proprios e metodos proprios.
data RtClass = RtClass
  { rcSuper   :: Maybe Name
  , rcFields  :: [Name]
  , rcMethods :: Map.Map Name FunDef
  }

data Ctx = Ctx
  { cxConsole :: Console
  , cxClasses :: Map.Map Name RtClass
  , cxGlobals :: Scope
  , cxInFun   :: Bool   -- dentro de funcao, a atribuicao nao ve o global
  }

-- | Como um comando terminou.
data Flow
  = Normal
  | Break
  | Continue
  | Return Value

-- | Registra funcoes e classes no escopo global e depois executa os
-- comandos de nivel superior na ordem do fonte (secao 3.6).
runProgram :: Console -> Program -> IO (Either Error ())
runProgram con (Program ds) = do
  globais <- newIORef Map.empty
  let classes = Map.fromList
        [ (clsName c, RtClass (clsSuper c) (map fieldName (clsFields c))
                              (Map.fromList [(fnName m, m) | m <- clsMethods c]))
        | DClass c <- ds ]
      ctx = Ctx con classes globais False
  forM_ ds $ \d -> case d of
    DFun f   -> define globais (fnName f) (VFun f [globais])
    DClass c -> define globais (clsName c) (VClass (clsName c))
    DStmt _  -> pure ()
  r <- try (forM_ [s | DStmt s <- ds] (exec ctx [globais]))
  pure $ case r of
    Left (RuntimeError p m) -> Left (Error p m)
    Right ()                -> Right ()

-- Escopos ------------------------------------------------------------------

newScope :: [(Name, Value)] -> IO Scope
newScope binds = do
  refs <- forM binds $ \(n, v) -> (,) n <$> newIORef v
  newIORef (Map.fromList refs)

define :: Scope -> Name -> Value -> IO ()
define sc n v = do
  ref <- newIORef v
  modifyIORef' sc (Map.insert n ref)

-- | Declara no escopo mais interno.
declareIn :: [Scope] -> Name -> Value -> IO ()
declareIn env n v = case env of
  sc : _ -> define sc n v
  []     -> pure ()

lookupRef :: [Scope] -> Name -> IO (Maybe (IORef Value))
lookupRef [] _ = pure Nothing
lookupRef (sc : resto) n = do
  m <- readIORef sc
  maybe (lookupRef resto n) (pure . Just) (Map.lookup n m)

-- Comandos -----------------------------------------------------------------

exec :: Ctx -> [Scope] -> Stmt -> IO Flow
exec ctx env s = case s of
  SVarDecl _ n _ e -> do
    v <- eval ctx env e
    Normal <$ declareIn env n v
  SAssign _ lv e -> do
    v <- eval ctx env e
    Normal <$ assign ctx env lv v
  SAugAssign p lv op e -> do
    (ler, escrever) <- place ctx env lv
    antigo <- ler
    v <- eval ctx env e
    novo <- binop p (augBinOp op) antigo v
    Normal <$ escrever novo
  SIf _ c t elifs e -> escolhe ((c, t) : [(c', b) | (_, c', b) <- elifs])
    where
      escolhe [] = block ctx env e
      escolhe ((cond, b) : resto) = do
        v <- eval ctx env cond
        if truthy v then block ctx env b else escolhe resto
  SWhile _ c b -> laco
    where
      laco = do
        v <- eval ctx env c
        if not (truthy v) then pure Normal else do
          f <- block ctx env b
          case f of
            Break    -> pure Normal
            Return x -> pure (Return x)
            _        -> laco
  SFor p v e b -> do
    col <- eval ctx env e
    -- a lista e lida de novo a cada volta: o corpo pode alterar a lista
    proximo <- case col of
      VList ref -> pure $ \i -> do
        xs <- readIORef ref
        pure (if i < Seq.length xs then Just (Seq.index xs i) else Nothing)
      VStr str -> pure $ \i ->
        pure (if i < length str then Just (VStr [str !! i]) else Nothing)
      _ -> erro p "'for' requires a list or a string"
    let laco i = do
          mx <- proximo i
          case mx of
            Nothing -> pure Normal
            Just x -> do
              sc <- newScope [(v, x)]
              f <- stmts ctx (sc : env) b
              case f of
                Break    -> pure Normal
                Return r -> pure (Return r)
                _        -> laco (i + 1)
    laco 0
  SReturn _ me -> Return <$> maybe (pure VNone) (eval ctx env) me
  SBreak _ -> pure Break
  SContinue _ -> pure Continue
  SPass _ -> pure Normal
  SExpr _ e -> Normal <$ eval ctx env e
  SFunDef f -> do
    -- a funcao local enxerga a si mesma, para a recursao, e o global
    propria <- newIORef Map.empty
    let fv = VFun f [propria, cxGlobals ctx]
    define propria (fnName f) fv
    Normal <$ declareIn env (fnName f) fv

-- | Executa os comandos em sequencia ate um deles desviar o fluxo.
stmts :: Ctx -> [Scope] -> [Stmt] -> IO Flow
stmts _ _ [] = pure Normal
stmts ctx env (s : resto) = do
  f <- exec ctx env s
  case f of
    Normal -> stmts ctx env resto
    _      -> pure f

block :: Ctx -> [Scope] -> Block -> IO Flow
block ctx env b = do
  sc <- newScope []
  stmts ctx (sc : env) b

-- | Atribuicao a nome que nao existe declara a variavel no escopo
-- corrente, como no verificador. Dentro de funcao o escopo global (o
-- ultimo da lista) fica de fora, como no Python.
assign :: Ctx -> [Scope] -> LValue -> Value -> IO ()
assign ctx env lv v = case lv of
  LVar _ n -> do
    let visiveis = if cxInFun ctx then take (length env - 1) env else env
    r <- lookupRef visiveis n
    maybe (declareIn env n v) (`writeIORef` v) r
  LIndex p b i -> do
    xs <- eval ctx env b
    idx <- eval ctx env i
    setIndex p xs idx v
  LField p o f -> do
    obj <- eval ctx env o
    ref <- fieldRef p obj f
    writeIORef ref v

-- | Leitura e escrita do alvo de uma atribuicao aumentada. A lista, o
-- indice e o objeto sao avaliados uma vez so, como no Python.
place :: Ctx -> [Scope] -> LValue -> IO (IO Value, Value -> IO ())
place ctx env lv = case lv of
  LVar p n -> do
    r <- lookupRef env n
    ref <- maybe (erro p ("undeclared variable: '" ++ n ++ "'")) pure r
    pure (readIORef ref, writeIORef ref)
  LField p o f -> do
    obj <- eval ctx env o
    ref <- fieldRef p obj f
    pure (readIORef ref, writeIORef ref)
  LIndex p b i -> do
    xs <- eval ctx env b
    idx <- eval ctx env i
    pure (index p xs idx, setIndex p xs idx)

-- Expressoes ---------------------------------------------------------------

eval :: Ctx -> [Scope] -> Expr -> IO Value
eval ctx env e = case e of
  EInt _ n   -> pure (VInt n)
  EFloat _ d -> pure (VFloat d)
  EStr _ s   -> pure (VStr s)
  EBool _ b  -> pure (VBool b)
  ENone _    -> pure VNone
  EVar p n   -> variable p n
  ESelf p    -> variable p "self"
  -- and e or em curto-circuito
  EBinOp _ And a b -> do
    x <- eval ctx env a
    if truthy x then eval ctx env b else pure x
  EBinOp _ Or a b -> do
    x <- eval ctx env a
    if truthy x then pure x else eval ctx env b
  EBinOp p op a b -> do
    x <- eval ctx env a
    y <- eval ctx env b
    binop p op x y
  EUnOp p op a -> do
    x <- eval ctx env a
    case (op, x) of
      (Neg, VInt n)   -> pure (VInt (negate n))
      (Neg, VFloat d) -> pure (VFloat (negate d))
      (Not, _)        -> pure (VBool (not (truthy x)))
      _               -> erro p "operator '-' requires a number"
  ECall p f args -> do
    fv <- eval ctx env f
    vs <- mapM (eval ctx env) args
    apply ctx p fv vs
  EIndex p b i -> do
    xs <- eval ctx env b
    idx <- eval ctx env i
    index p xs idx
  EField p o f -> do
    obj <- eval ctx env o
    member ctx p obj f
  EList _ es -> do
    vs <- mapM (eval ctx env) es
    VList <$> newIORef (Seq.fromList vs)
  ELambda _ ps body -> pure (VLambda ps body env)
  ECond _ a c b -> do
    v <- eval ctx env c
    if truthy v then eval ctx env a else eval ctx env b
  where
    variable p n = do
      r <- lookupRef env n
      case r of
        Just ref -> readIORef ref
        Nothing
          | n `elem` builtinNames -> pure (VBuiltin n)
          | otherwise -> erro p ("undeclared variable: '" ++ n ++ "'")

builtinNames :: [Name]
builtinNames = ["print", "input", "int", "float", "str", "bool", "len", "range", "append"]

truthy :: Value -> Bool
truthy v = case v of
  VBool b  -> b
  VInt n   -> n /= 0
  VFloat d -> d /= 0
  VStr s   -> not (null s)
  VNone    -> False
  _        -> True

-- | Chamada: funcao, lambda, metodo ligado, construtor ou primitiva.
apply :: Ctx -> Pos -> Value -> [Value] -> IO Value
apply ctx p fv vs = case fv of
  VFun fd fora -> invoke ctx fd fora vs
  VLambda ps body capturados -> do
    sc <- newScope (zip ps vs)
    eval ctx (sc : capturados) body
  VPartial f pre -> apply ctx p f (pre ++ vs)
  VClass c -> construct ctx p c vs
  VBuiltin n -> builtin ctx p n vs
  _ -> erro p "value is not a function"

-- | Os parametros e as declaracoes do corpo ficam no mesmo escopo.
invoke :: Ctx -> FunDef -> [Scope] -> [Value] -> IO Value
invoke ctx fd fora vs = do
  sc <- newScope (zip (map paramName (fnParams fd)) vs)
  f <- stmts ctx { cxInFun = True } (sc : fora) (fnBody fd)
  pure $ case f of
    Return v -> v
    _        -> VNone

-- | Metodo procurado a partir da classe dada, subindo pelos pais.
findMethod :: Ctx -> Name -> Name -> Maybe FunDef
findMethod ctx c m = do
  rc <- Map.lookup c (cxClasses ctx)
  case Map.lookup m (rcMethods rc) of
    Just fd -> Just fd
    Nothing -> rcSuper rc >>= \s -> findMethod ctx s m

allFields :: Ctx -> Name -> [Name]
allFields ctx c = case Map.lookup c (cxClasses ctx) of
  Just rc -> rcFields rc ++ maybe [] (allFields ctx) (rcSuper rc)
  Nothing -> []

-- | Cria o objeto com todos os campos, proprios e herdados, e chama o
-- __init__ da classe ou do ancestral mais proximo.
construct :: Ctx -> Pos -> Name -> [Value] -> IO Value
construct ctx p c vs = do
  ident <- newUnique
  campos <- forM (allFields ctx c) $ \f -> (,) f <$> newIORef VNone
  let obj = VObject (Object ident c (Map.fromList campos))
  case findMethod ctx c "__init__" of
    Just ini -> void (invoke ctx ini [cxGlobals ctx] (obj : vs))
    Nothing
      | null vs   -> pure ()
      | otherwise -> erro p ("class '" ++ c ++ "' takes no arguments")
  pure obj

-- | obj.f. Metodo e buscado na classe concreta do objeto (despacho
-- dinamico); em Classe.m, na classe escrita, como em Pai.__init__.
member :: Ctx -> Pos -> Value -> Name -> IO Value
member ctx p v f = case v of
  VObject o -> case Map.lookup f (objFields o) of
    Just ref -> readIORef ref
    Nothing -> case findMethod ctx (objClass o) f of
      Just fd -> pure (VPartial (VFun fd [cxGlobals ctx]) [v])
      Nothing -> erro p ("object of class '" ++ objClass o ++ "' has no attribute '" ++ f ++ "'")
  VClass c -> case findMethod ctx c f of
    Just fd -> pure (VFun fd [cxGlobals ctx])
    Nothing -> erro p ("class '" ++ c ++ "' has no method '" ++ f ++ "'")
  VList _ | f == "append" -> pure (VPartial (VBuiltin "append") [v])
  _ -> erro p ("value has no attribute '" ++ f ++ "'")

fieldRef :: Pos -> Value -> Name -> IO (IORef Value)
fieldRef p v f = case v of
  VObject o -> maybe semCampo pure (Map.lookup f (objFields o))
    where semCampo = erro p ("object of class '" ++ objClass o ++ "' has no field '" ++ f ++ "'")
  _ -> erro p ("value has no field '" ++ f ++ "'")

-- | Acesso por indice, conferido em tempo de execucao (regra 5).
index :: Pos -> Value -> Value -> IO Value
index p col (VInt i) = case col of
  VList ref -> do
    xs <- readIORef ref
    if dentro (Seq.length xs)
      then pure (Seq.index xs (fromInteger i))
      else foraDaLista (Seq.length xs)
  VStr s
    | dentro (length s) -> pure (VStr [s !! fromInteger i])
    | otherwise -> erro p ("string index out of range: index " ++ show i
                           ++ ", length " ++ show (length s))
  _ -> erro p "indexing requires a list or a string"
  where
    dentro n = i >= 0 && i < toInteger n
    foraDaLista n = erro p ("list index out of range: index " ++ show i ++ ", length " ++ show n)
index p _ _ = erro p "index must be an integer"

setIndex :: Pos -> Value -> Value -> Value -> IO ()
setIndex p (VList ref) (VInt i) v = do
  xs <- readIORef ref
  if i >= 0 && i < toInteger (Seq.length xs)
    then writeIORef ref (Seq.update (fromInteger i) v xs)
    else erro p ("list index out of range: index " ++ show i ++ ", length " ++ show (Seq.length xs))
setIndex p _ _ _ = erro p "indexed assignment requires a list"

augBinOp :: AugOp -> BinOp
augBinOp op = case op of
  AddAssign      -> Add
  SubAssign      -> Sub
  MulAssign      -> Mul
  DivAssign      -> Div
  FloorDivAssign -> FloorDiv
  ModAssign      -> Mod

-- | Operadores sobre valores. / entre inteiros da float; // e % seguem
-- o Python, arredondando para baixo (div e mod do Haskell).
binop :: Pos -> BinOp -> Value -> Value -> IO Value
binop p op x y = case (op, x, y) of
  (Add, VInt a, VInt b)     -> pure (VInt (a + b))
  (Add, VFloat a, VFloat b) -> pure (VFloat (a + b))
  (Add, VStr a, VStr b)     -> pure (VStr (a ++ b))
  (Sub, VInt a, VInt b)     -> pure (VInt (a - b))
  (Sub, VFloat a, VFloat b) -> pure (VFloat (a - b))
  (Mul, VInt a, VInt b)     -> pure (VInt (a * b))
  (Mul, VFloat a, VFloat b) -> pure (VFloat (a * b))
  (Div, VInt a, VInt b)
    | b == 0    -> erro p "division by zero"
    | otherwise -> pure (VFloat (fromInteger a / fromInteger b))
  (Div, VFloat a, VFloat b)
    | b == 0    -> erro p "division by zero"
    | otherwise -> pure (VFloat (a / b))
  (FloorDiv, VInt a, VInt b)
    | b == 0    -> erro p "integer division by zero"
    | otherwise -> pure (VInt (a `div` b))
  (Mod, VInt a, VInt b)
    | b == 0    -> erro p "integer modulo by zero"
    | otherwise -> pure (VInt (a `mod` b))
  (Pow, VInt a, VInt b)
    | b < 0     -> erro p "negative exponent in integer power"
    | otherwise -> pure (VInt (a ^ b))
  (Pow, VFloat a, VFloat b) -> pure (VFloat (a ** b))
  (Eq, _, _)  -> VBool <$> equal x y
  (Neq, _, _) -> VBool . not <$> equal x y
  (Lt, _, _)  -> ordem (<) (<) (<)
  (Le, _, _)  -> ordem (<=) (<=) (<=)
  (Gt, _, _)  -> ordem (>) (>) (>)
  (Ge, _, _)  -> ordem (>=) (>=) (>=)
  _ -> erro p "unsupported operand types"
  where
    -- o mesmo operador para int, float e str
    ordem fi ff fs = case (x, y) of
      (VInt a, VInt b)     -> pure (VBool (fi a b))
      (VFloat a, VFloat b) -> pure (VBool (ff a b))
      (VStr a, VStr b)     -> pure (VBool (fs a b))
      _                    -> erro p "values cannot be ordered"

-- | Igualdade: estrutural para listas e por identidade para objetos.
equal :: Value -> Value -> IO Bool
equal x y = case (x, y) of
  (VInt a, VInt b)       -> pure (a == b)
  (VFloat a, VFloat b)   -> pure (a == b)
  (VStr a, VStr b)       -> pure (a == b)
  (VBool a, VBool b)     -> pure (a == b)
  (VNone, VNone)         -> pure True
  (VObject a, VObject b) -> pure (objId a == objId b)
  (VList a, VList b) -> do
    xs <- toList <$> readIORef a
    ys <- toList <$> readIORef b
    if length xs /= length ys then pure False else and <$> zipWithM equal xs ys
  _ -> pure False

-- Primitivas ---------------------------------------------------------------

-- | Funcoes primitivas da secao 8 da especificacao.
builtin :: Ctx -> Pos -> Name -> [Value] -> IO Value
builtin ctx p n vs = case (n, vs) of
  ("print", [v]) -> do
    s <- showValue v
    VNone <$ conWrite (cxConsole ctx) s
  ("input", []) -> do
    linha <- conRead (cxConsole ctx)
    case linha of
      Just l  -> pure (VStr (filter (/= '\r') l))
      Nothing -> erro p "EOF when reading a line"
  ("int", [VStr s]) ->
    maybe (erro p ("invalid literal for int(): '" ++ s ++ "'")) (pure . VInt) (parseInt s)
  ("int", [VFloat d])
    | isNaN d || isInfinite d -> erro p ("cannot convert float " ++ showFloat d ++ " to integer")
    | otherwise -> pure (VInt (truncate d))
  ("int", [VInt i]) -> pure (VInt i)
  ("int", [VBool b]) -> pure (VInt (if b then 1 else 0))
  ("float", [VStr s]) ->
    maybe (erro p ("could not convert string to float: '" ++ s ++ "'")) (pure . VFloat) (parseFloat s)
  ("float", [VInt i]) -> pure (VFloat (fromInteger i))
  ("float", [VFloat d]) -> pure (VFloat d)
  ("str", [v]) -> VStr <$> showValue v
  ("bool", [v]) -> pure (VBool (truthy v))
  ("len", [VList ref]) -> VInt . toInteger . Seq.length <$> readIORef ref
  ("len", [VStr s]) -> pure (VInt (toInteger (length s)))
  ("range", _) -> do
    ns <- mapM inteiro vs
    xs <- case ns of
      [fim]               -> pure [0 .. fim - 1]
      [ini, fim]          -> pure [ini .. fim - 1]
      [ini, fim, passo]
        | passo > 0 -> pure (takeWhile (< fim) [ini, ini + passo ..])
        | passo < 0 -> pure (takeWhile (> fim) [ini, ini + passo ..])
        | otherwise -> erro p "range() step must not be zero"
      _ -> erro p "range() expects 1 to 3 arguments"
    VList <$> newIORef (Seq.fromList (map VInt xs))
  ("append", [VList ref, v]) -> VNone <$ modifyIORef' ref (Seq.|> v)
  _ -> erro p ("invalid arguments to '" ++ n ++ "'")
  where
    inteiro (VInt i) = pure i
    inteiro _        = erro p "range() expects integers"
