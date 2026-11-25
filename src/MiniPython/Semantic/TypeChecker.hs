-- | Verificador de tipos do MiniPython (secao 10 da especificacao).
--
-- A verificacao e feita por unificacao, com incognitas (TMeta) para o
-- que nao tem anotacao: parametros de lambda, lista vazia, funcao sem
-- anotacao e as variaveis de tipo de uma funcao polimorfica quando ela
-- e chamada. A subtipagem entre classes entra so onde um valor e usado
-- no lugar de outro (declaracao, atribuicao, argumento e retorno), em
-- subtype; o resto e igualdade de tipos.
--
-- O verificador nao para no primeiro erro: cada erro e guardado e a
-- expressao com erro recebe um tipo razoavel (ou uma incognita), para
-- a verificacao seguir sem erros em cascata.
module MiniPython.Semantic.TypeChecker
  ( typeCheck
  ) where

import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.State.Strict (State, execState, get, gets, modify, put)
import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import MiniPython.Frontend.Lexer.Token (Pos)
import MiniPython.Frontend.Syntax.MiniPythonSyntax
import MiniPython.Semantic.Types
import MiniPython.Utils.Error

-- Ambiente ----------------------------------------------------------------

-- | O que um nome significa num escopo.
data Binding
  = BVar Ty        -- variavel ou parametro
  | BFun Scheme    -- funcao declarada com def
  | BClass         -- nome de classe, usado como construtor

-- | Ambiente de classes: pai, campos e metodos, ja com os herdados.
data ClassInfo = ClassInfo
  { ciSuper   :: Maybe Name
  , ciFields  :: Map.Map Name (Name, Ty)   -- campo -> (classe dona, tipo)
  , ciMethods :: Map.Map Name Method
  , ciOwn     :: [Method]                  -- os da propria classe, na ordem
  }

-- | Assinatura de metodo, sem o self.
data Method = Method
  { mOwner  :: Name
  , mParams :: [(Name, Ty)]
  , mRet    :: Ty
  }

-- | Funcao em que o comando esta: tipo de retorno e, se for metodo, a
-- classe de self.
data FunCtx = FunCtx
  { fcReturn :: Ty
  , fcClass  :: Maybe Name
  }

data St = St
  { stSubst   :: IntMap.IntMap Ty
  , stNext    :: Int
  , stErrors  :: [Error]                 -- em ordem inversa
  , stScopes  :: [Map.Map Name Binding]  -- escopos locais, o mais interno primeiro
  , stGlobals :: Map.Map Name Binding
  , stClasses :: Map.Map Name ClassInfo
  , stFun     :: Maybe FunCtx
  , stLoop    :: Bool
  }

type TC = State St

-- | Erros do programa, na ordem em que foram encontrados. Lista vazia
-- quando o programa esta bem tipado.
typeCheck :: Program -> [Error]
typeCheck (Program ds) = reverse (stErrors (execState (program ds) inicial))
  where
    inicial = St IntMap.empty 0 [] [] Map.empty Map.empty Nothing False

report :: Pos -> String -> TC ()
report p msg = modify (\s -> s { stErrors = Error p msg : stErrors s })

fresh :: TC Ty
fresh = do
  n <- gets stNext
  modify (\s -> s { stNext = n + 1 })
  pure (TMeta n)

lookupName :: Name -> TC (Maybe Binding)
lookupName n = do
  st <- get
  pure (listToMaybe (mapMaybe (Map.lookup n) (stScopes st ++ [stGlobals st])))

-- | Nome visivel para atribuicao. Dentro de funcao so valem os escopos
-- locais: como no Python sem global, x = e com x global declara uma
-- variavel local, e a global nao muda.
lookupAssignable :: Name -> TC (Maybe Binding)
lookupAssignable n = do
  st <- get
  let escopos = case stFun st of
        Just _  -> stScopes st
        Nothing -> stScopes st ++ [stGlobals st]
  pure (listToMaybe (mapMaybe (Map.lookup n) escopos))

-- | Declara no escopo corrente; fora de funcao e de bloco, no global.
declare :: Pos -> Name -> Binding -> TC ()
declare p n b = do
  st <- get
  case stScopes st of
    sc : resto
      | Map.member n sc -> repetido
      | otherwise       -> put st { stScopes = Map.insert n b sc : resto }
    []
      | Map.member n (stGlobals st) -> repetido
      | otherwise -> put st { stGlobals = Map.insert n b (stGlobals st) }
  where
    repetido = report p ("'" ++ n ++ "' is already declared in this scope")

withScope :: Map.Map Name Binding -> TC a -> TC a
withScope sc m = do
  modify (\s -> s { stScopes = sc : stScopes s })
  r <- m
  modify (\s -> s { stScopes = drop 1 (stScopes s) })
  pure r

inLoop :: TC a -> TC a
inLoop m = do
  antes <- gets stLoop
  modify (\s -> s { stLoop = True })
  r <- m
  modify (\s -> s { stLoop = antes })
  pure r

-- | Corpo de funcao: os escopos locais de fora deixam de valer, porque
-- def nao forma closure. O global continua visivel.
inFunction :: FunCtx -> [Map.Map Name Binding] -> TC a -> TC a
inFunction ctx scopes m = do
  antes <- get
  put antes { stScopes = scopes, stFun = Just ctx, stLoop = False }
  r <- m
  modify (\s -> s { stScopes = stScopes antes, stFun = stFun antes, stLoop = stLoop antes })
  pure r

classInfo :: Name -> TC (Maybe ClassInfo)
classInfo c = gets (Map.lookup c . stClasses)

lookupField :: Name -> Name -> TC (Maybe Ty)
lookupField c f = (>>= fmap snd . Map.lookup f . ciFields) <$> classInfo c

lookupMethod :: Name -> Name -> TC (Maybe Method)
lookupMethod c m = (>>= Map.lookup m . ciMethods) <$> classInfo c

methodType :: Method -> Ty
methodType m = TFun (map snd (mParams m)) (mRet m)

-- Unificacao e subtipagem ---------------------------------------------------

-- | Segue a substituicao enquanto o tipo for uma incognita resolvida.
shallow :: Ty -> TC Ty
shallow t@(TMeta i) = do
  s <- gets stSubst
  maybe (pure t) shallow (IntMap.lookup i s)
shallow t = pure t

-- | Aplica a substituicao no tipo inteiro.
zonk :: Ty -> TC Ty
zonk t = do
  t' <- shallow t
  case t' of
    TList a   -> TList <$> zonk a
    TFun ps r -> TFun <$> mapM zonk ps <*> zonk r
    _         -> pure t'

data Falha
  = Diferentes Ty Ty
  | Ciclo Int Ty

-- | Unifica e devolve o primeiro par que nao casou, o mais interno.
unifyM :: Ty -> Ty -> TC (Maybe Falha)
unifyM a b = do
  a' <- shallow a
  b' <- shallow b
  case (a', b') of
    (TMeta i, TMeta j) | i == j -> pure Nothing
    (TMeta i, t) -> ligar i t
    (t, TMeta i) -> ligar i t
    (TList x, TList y) -> unifyM x y
    (TFun xs r, TFun ys s)
      | length xs == length ys -> pares (zip (xs ++ [r]) (ys ++ [s]))
    _ | a' == b'   -> pure Nothing
      | otherwise -> pure (Just (Diferentes a' b'))
  where
    pares [] = pure Nothing
    pares ((x, y) : resto) = unifyM x y >>= maybe (pares resto) (pure . Just)
    -- occurs check (regra 21): ?0 nao pode valer list[?0]
    ligar i t = do
      t' <- zonk t
      if i `elem` metas t'
        then pure (Just (Ciclo i t'))
        else Nothing <$ modify (\s -> s { stSubst = IntMap.insert i t' (stSubst s) })

unify :: Pos -> Ty -> Ty -> TC ()
unify p esperado obtido = unifyM esperado obtido >>= mapM_ falha
  where
    falha (Diferentes a b) = incompatible p a b
    falha (Ciclo i t) = do
      (si, st) <- quoted (TMeta i) t
      report p ("infinite type: '" ++ si ++ "' occurs in '" ++ st ++ "'")

-- | Tenta unificar sem reportar; se falhar, desfaz o que foi ligado.
tryUnify :: Ty -> Ty -> TC Bool
tryUnify a b = do
  antes <- gets stSubst
  r <- unifyM a b
  case r of
    Nothing -> pure True
    Just _  -> False <$ modify (\s -> s { stSubst = antes })

incompatible :: Pos -> Ty -> Ty -> TC ()
incompatible p a b = do
  (sa, sb) <- quoted a b
  report p ("incompatible types: '" ++ sa ++ "' and '" ++ sb ++ "'")

-- | Os dois tipos de uma mensagem, com as incognitas numeradas juntas.
quoted :: Ty -> Ty -> TC (String, String)
quoted a b = do
  a' <- zonk a
  b' <- zonk b
  pure $ case showTys [a', b'] of
    [sa, sb] -> (sa, sb)
    _        -> (showTy a', showTy b')

showOne :: Ty -> TC String
showOne t = concat . showTys . pure <$> zonk t

-- | Regras 9 e 12: d <: c se c e d ou um ancestral de d.
subclass :: Name -> Name -> TC Bool
subclass d c
  | d == c    = pure True
  | otherwise = do
      ci <- classInfo d
      maybe (pure False) (`subclass` c) (ci >>= ciSuper)

-- | Um valor do tipo obtido pode ser usado onde se espera o outro
-- (regra 16). Fora de classe, os tipos tem que ser iguais.
subtype :: Pos -> Ty -> Ty -> TC ()
subtype p esperado obtido = do
  e <- shallow esperado
  o <- shallow obtido
  case (e, o) of
    (TClass c, TClass d) -> do
      ok <- subclass d c
      unless ok (incompatible p e o)
    _ -> unify p e o

-- | Menor tipo que serve aos dois: o ancestral comum, para classes.
join :: Pos -> Ty -> Ty -> TC Ty
join p a b = do
  a' <- shallow a
  b' <- shallow b
  case (a', b') of
    (TClass c, TClass d) -> do
      ancestral <- comum c d
      case ancestral of
        Just k  -> pure (TClass k)
        Nothing -> a' <$ incompatible p a' b'
    _ -> a' <$ unify p a' b'
  where
    comum c d = do
      ok <- subclass d c
      if ok then pure (Just c) else do
        ci <- classInfo c
        maybe (pure Nothing) (`comum` d) (ci >>= ciSuper)

-- | Funcao polimorfica: troca as variaveis quantificadas por incognitas.
instantiate :: Scheme -> TC Ty
instantiate (Forall [] t) = pure t
instantiate (Forall vs t) = do
  ms <- mapM (const fresh) vs
  pure (substGen (Map.fromList (zip vs ms)) t)

-- | Quantifica as variaveis de tipo escritas na assinatura.
schemeOf :: [(Name, Ty)] -> Ty -> Scheme
schemeOf params r = Forall (gens t) t
  where
    t = TFun (map snd params) r

-- | Regra 19: depois de verificar uma funcao de nivel superior, as
-- incognitas que sobraram na assinatura, e que nao aparecem em outro
-- nome global, viram variaveis de tipo.
generalize :: Name -> Ty -> TC Scheme
generalize nome t = do
  t' <- zonk t
  globais <- gets (Map.toList . Map.delete nome . stGlobals)
  presas <- concat <$> mapM (fmap metas . zonk . tipoDe . snd) globais
  let livres = filter (`notElem` presas) (metas t')
      usadas = gens t'
      nomes = take (length livres) (filter (`notElem` usadas) candidatos)
      troca = Map.fromList (zip livres nomes)
  pure (Forall (usadas ++ nomes) (renomeia troca t'))
  where
    candidatos = [[c] | c <- ['a' .. 'z']] ++ ['t' : show i | i <- [1 :: Int ..]]
    tipoDe b = case b of
      BVar ty              -> ty
      BFun (Forall _ ty)   -> ty
      BClass               -> TNone
    renomeia troca ty = case ty of
      TMeta i   -> maybe ty TGen (Map.lookup i troca)
      TList a   -> TList (renomeia troca a)
      TFun ps r -> TFun (map (renomeia troca) ps) (renomeia troca r)
      _         -> ty

-- Anotacoes e assinaturas -------------------------------------------------

convType :: Type -> TC Ty
convType ty = case ty of
  TyInt _      -> pure TInt
  TyFloat _    -> pure TFloat
  TyBool _     -> pure TBool
  TyStr _      -> pure TStr
  TyNone _     -> pure TNone
  TyList _ a   -> TList <$> convType a
  TyFun _ ps r -> TFun <$> mapM convType ps <*> convType r
  TyVar _ n    -> pure (TGen n)
  TyClass p n  -> do
    b <- gets (Map.lookup n . stGlobals)
    case b of
      Just BClass -> pure (TClass n)
      _           -> report p ("undeclared class: '" ++ n ++ "'") >> fresh

-- | Parametros e retorno; o que nao tem anotacao vira incognita.
signature :: FunDef -> [Param] -> TC ([(Name, Ty)], Ty)
signature fd ps = do
  params <- forM ps $ \p -> do
    t <- maybe fresh convType (paramType p)
    pure (paramName p, t)
  r <- maybe fresh convType (fnReturn fd)
  pure (params, r)

-- | Regra 14: self so existe em metodo. Fora de metodo o erro sai no
-- parametro, que continua na assinatura para nao gerar erro de aridade
-- em cada chamada.
funParams :: FunDef -> TC [Param]
funParams fd = do
  forM_ (fnParams fd) $ \p ->
    when (paramName p == "self") (report (paramPos p) "'self' outside a method")
  pure (fnParams fd)

-- | Parametros de metodo sem o self, que tem que ser o primeiro.
methodParams :: Name -> FunDef -> TC [Param]
methodParams cls fd = case fnParams fd of
  p : ps | paramName p == "self" -> semSelf ps
  ps -> do
    report (fnPos fd) ("method '" ++ fnName fd ++ "' of class '" ++ cls
                       ++ "' must take 'self' as its first parameter")
    semSelf ps
  where
    semSelf ps = do
      forM_ [p | p <- ps, paramName p == "self"] $ \p ->
        report (paramPos p) "'self' must be the first parameter of a method"
      pure [p | p <- ps, paramName p /= "self"]

-- Programa, classes e funcoes ----------------------------------------------

-- | Funcoes e classes de nivel superior entram no escopo global antes de
-- qualquer corpo ser verificado, o que permite recursao e referencia
-- mutua. Depois cada item e verificado na ordem do fonte.
program :: [Decl] -> TC ()
program ds = do
  let classes = [c | DClass c <- ds]
  forM_ classes $ \c -> declare (clsPos c) (clsName c) BClass
  buildClasses classes
  sigs <- forM [f | DFun f <- ds] $ \f -> do
    ps <- funParams f
    sig@(params, r) <- signature f ps
    declare (fnPos f) (fnName f) (BFun (schemeOf params r))
    pure sig
  go ds sigs
  where
    go [] _ = pure ()
    go (DFun f : resto) (sig : sigs) = topFunction f sig >> go resto sigs
    go (DFun _ : resto) [] = go resto []
    go (DClass c : resto) sigs = checkClass c >> go resto sigs
    go (DStmt s : resto) sigs = stmt s >> go resto sigs

topFunction :: FunDef -> ([(Name, Ty)], Ty) -> TC ()
topFunction f (params, r) = do
  funBody f Nothing params r []
  let t = TFun (map snd params) r
  t' <- zonk t
  unless (null (metas t')) $ do
    sc <- generalize (fnName f) t
    modify (\s -> s { stGlobals = Map.insert (fnName f) (BFun sc) (stGlobals s) })

-- | Funcao local: nao e generalizada (regra 19) e so ve o global e a si
-- mesma.
localFunction :: FunDef -> TC ()
localFunction f = do
  ps <- funParams f
  (params, r) <- signature f ps
  let sc = schemeOf params r
  funBody f Nothing params r [Map.singleton (fnName f) (BFun sc)]
  declare (fnPos f) (fnName f) (BFun sc)

-- | Corpo de funcao ou metodo; os parametros ficam no mesmo escopo das
-- declaracoes do corpo.
funBody :: FunDef -> Maybe Name -> [(Name, Ty)] -> Ty -> [Map.Map Name Binding] -> TC ()
funBody f cls params r fora = do
  let escopo = Map.fromList [(n, BVar t) | (n, t) <- params]
  inFunction (FunCtx r cls) (escopo : fora) (mapM_ stmt (fnBody f))
  -- regra 5: funcao que nao devolve None tem return em todo caminho
  unless (alwaysReturns (fnBody f)) $ do
    r' <- shallow r
    case r' of
      TNone   -> pure ()
      TMeta _ -> unify (fnPos f) TNone r'
      _       -> report (fnPos f) ("missing return in " ++ oque ++ " '" ++ fnName f ++ "'")
  where
    oque = maybe "function" (const "method") cls

-- | O bloco termina em return em todos os caminhos. while True sem break
-- tambem conta, porque nunca chega ao fim.
alwaysReturns :: Block -> Bool
alwaysReturns = any retorna
  where
    retorna s = case s of
      SReturn _ _ -> True
      SIf _ _ t elifs e ->
        not (null e) && all alwaysReturns (t : [b | (_, _, b) <- elifs] ++ [e])
      SWhile _ (EBool _ True) b -> not (temBreak b)
      _ -> False
    temBreak = any quebra
    quebra s = case s of
      SBreak _ -> True
      SIf _ _ t elifs e -> any temBreak (t : [b | (_, _, b) <- elifs] ++ [e])
      _ -> False

-- | Monta o ambiente de classes. O pai e montado antes do filho, mesmo
-- que venha depois no fonte; heranca circular e cortada.
buildClasses :: [ClassDef] -> TC ()
buildClasses cs = mapM_ (void . info []) cs
  where
    defs = Map.fromListWith (\_ primeiro -> primeiro) [(clsName c, c) | c <- cs]
    info visitando c = do
      pronto <- classInfo (clsName c)
      case pronto of
        Just ci -> pure (Just ci)
        Nothing -> do
          pai <- case clsSuper c of
            Nothing -> pure Nothing
            Just s
              | s `elem` (clsName c : visitando) -> do
                  report (clsPos c) ("cyclic inheritance in class '" ++ clsName c ++ "'")
                  pure Nothing
              | otherwise -> case Map.lookup s defs of
                  Nothing -> do
                    report (clsPos c) ("undeclared class: '" ++ s ++ "'")
                    pure Nothing
                  Just sc -> fmap ((,) s) <$> info (clsName c : visitando) sc
          ci <- members c pai
          modify (\st -> st { stClasses = Map.insert (clsName c) ci (stClasses st) })
          pure (Just ci)

-- | Campos e metodos da classe somados aos herdados. Aqui ficam as
-- regras 7 (nome repetido) e 10 (override com a mesma assinatura).
members :: ClassDef -> Maybe (Name, ClassInfo) -> TC ClassInfo
members c pai = do
  campos <- foldM campo (maybe Map.empty (ciFields . snd) pai) (clsFields c)
  proprios <- forM (clsMethods c) $ \m -> do
    ps <- methodParams (clsName c) m
    (params, r) <- signature m ps
    when (fnName m == "__init__") (unify (fnPos m) TNone r)
    pure (Method (clsName c) params r)
  metodos <- foldM (metodo campos) (maybe Map.empty (ciMethods . snd) pai)
                   (zip (clsMethods c) proprios)
  pure (ClassInfo (fst <$> pai) campos metodos proprios)
  where
    campo acc f = case Map.lookup (fieldName f) acc of
      Just (dono, _) -> do
        report (fieldPos f) ("field '" ++ fieldName f ++ "' is already declared in class '" ++ dono ++ "'")
        pure acc
      Nothing -> do
        t <- convType (fieldType f)
        pure (Map.insert (fieldName f) (clsName c, t) acc)
    metodo campos acc (m, mi)
      | Map.member (fnName m) campos = do
          report (fnPos m) ("'" ++ fnName m ++ "' is already declared in class '" ++ clsName c ++ "'")
          pure acc
      | otherwise = case Map.lookup (fnName m) acc of
          Just anterior
            | mOwner anterior == clsName c -> do
                report (fnPos m) ("method '" ++ fnName m ++ "' is already declared in class '" ++ clsName c ++ "'")
                pure acc
            | fnName m /= "__init__" -> do
                ok <- tryUnify (methodType anterior) (methodType mi)
                unless ok $ do
                  (sa, sb) <- quoted (methodType anterior) (methodType mi)
                  report (fnPos m) ("method '" ++ fnName m ++ "' overrides '" ++ mOwner anterior
                                    ++ "." ++ fnName m ++ "' with a different signature: expected '"
                                    ++ sa ++ "', got '" ++ sb ++ "'")
                pure (Map.insert (fnName m) mi acc)
          _ -> pure (Map.insert (fnName m) mi acc)

checkClass :: ClassDef -> TC ()
checkClass c = do
  ci <- classInfo (clsName c)
  forM_ ci $ \info ->
    forM_ (zip (clsMethods c) (ciOwn info)) $ \(m, mi) ->
      funBody m (Just (clsName c)) (mParams mi) (mRet mi) []
  initialized c

-- | Regra 13: o __init__ de uma classe com campos atribui todos eles.
initialized :: ClassDef -> TC ()
initialized c = unless (null (clsFields c)) $
  case [m | m <- clsMethods c, fnName m == "__init__"] of
    [] -> report (clsPos c) ("class '" ++ clsName c ++ "' declares fields but has no '__init__'")
    ini : _ -> forM_ (clsFields c) $ \f ->
      unless (fieldName f `elem` atribuidos (fnBody ini)) $
        report (fieldPos f) ("field '" ++ fieldName f ++ "' is not initialized in '__init__'")
  where
    atribuidos = concatMap campos
    campos s = case s of
      SAssign _ (LField _ (ESelf _) f) _ -> [f]
      SIf _ _ t elifs e -> atribuidos (t ++ concat [b | (_, _, b) <- elifs] ++ e)
      SWhile _ _ b -> atribuidos b
      SFor _ _ _ b -> atribuidos b
      _ -> []

-- Comandos -----------------------------------------------------------------

stmt :: Stmt -> TC ()
stmt s = case s of
  SVarDecl p n ty e -> do
    t <- convType ty
    check p t e
    declare p n (BVar t)
  SAssign p lv e -> assign p lv e
  SAugAssign p lv op e -> do
    case lv of
      LVar vp n -> do
        local <- lookupAssignable n
        global <- lookupName n
        case (local, global) of
          (Nothing, Just (BVar _)) ->
            report vp ("global variable '" ++ n ++ "' cannot be modified inside a function")
          _ -> pure ()
      _ -> pure ()
    alvo <- infer (lvalueExpr lv)
    te <- infer e
    tr <- binop p (augName op) (augBinOp op) alvo te
    subtype p alvo tr
  SIf _ c t elifs e -> do
    cond c
    block t
    forM_ elifs $ \(_, c', b) -> cond c' >> block b
    block e
  SWhile _ c b -> cond c >> inLoop (block b)
  SFor _ v e b -> do
    te <- infer e
    elemento <- elementOf (exprPos e) te
    -- a variavel do laco fica no escopo do corpo
    inLoop (withScope (Map.singleton v (BVar elemento)) (mapM_ stmt b))
  SReturn p me -> do
    ctx <- gets stFun
    case (ctx, me) of
      (Nothing, _) -> report p "'return' outside a function" >> mapM_ infer me
      (Just fc, Nothing) -> subtype p (fcReturn fc) TNone
      (Just fc, Just e) -> check p (fcReturn fc) e
  SBreak p -> do
    dentro <- gets stLoop
    unless dentro (report p "'break' outside a loop")
  SContinue p -> do
    dentro <- gets stLoop
    unless dentro (report p "'continue' outside a loop")
  SPass _ -> pure ()
  SExpr _ e -> void (infer e)
  SFunDef f -> localFunction f

-- | Cada bloco abre um escopo, descartado na saida.
block :: Block -> TC ()
block b = withScope Map.empty (mapM_ stmt b)

cond :: Expr -> TC ()
cond c = infer c >>= subtype (exprPos c) TBool

-- | for itera sobre lista (range devolve list[int]) ou string.
elementOf :: Pos -> Ty -> TC Ty
elementOf p t = do
  t' <- shallow t
  case t' of
    TList a -> pure a
    TStr    -> pure TStr
    TMeta _ -> do
      a <- fresh
      unify p t' (TList a)
      pure a
    _ -> do
      st <- showOne t'
      report p ("'for' requires 'list[T]' or 'str', got '" ++ st ++ "'")
      fresh

-- | Atribuicao a nome ainda nao declarado e declaracao com o tipo
-- inferido (secao 6.1 da especificacao).
assign :: Pos -> LValue -> Expr -> TC ()
assign p lv e = case lv of
  LVar vp n -> do
    b <- lookupAssignable n
    case b of
      Just (BVar t) -> check p t e
      Just (BFun _) -> report vp ("cannot assign to function '" ++ n ++ "'") >> void (infer e)
      Just BClass   -> report vp ("cannot assign to class '" ++ n ++ "'") >> void (infer e)
      Nothing       -> do
        -- regra 18: o tipo tem que sair do inicializador; [] sozinho
        -- precisa de anotacao (secao 6.8)
        t <- infer e >>= zonk
        unless (null (metas t)) $
          report vp ("cannot infer the type of '" ++ n ++ "'")
        declare vp n (BVar t)
  LIndex ip b i -> do
    tb <- infer b >>= shallow
    ti <- infer i
    case tb of
      TList a -> indexType ip ti >> check p a e
      TMeta _ -> do
        a <- fresh
        unify ip tb (TList a)
        indexType ip ti
        check p a e
      _ -> do
        st <- showOne tb
        report ip ("indexed assignment requires 'list[T]', got '" ++ st ++ "'")
        indexType ip ti
        void (infer e)
  LField fp o f -> do
    t <- infer o >>= shallow
    mf <- fieldOf fp t f
    maybe (void (infer e)) (\ft -> check p ft e) mf

lvalueExpr :: LValue -> Expr
lvalueExpr lv = case lv of
  LVar p n     -> EVar p n
  LIndex p b i -> EIndex p b i
  LField p b f -> EField p b f

augBinOp :: AugOp -> BinOp
augBinOp op = case op of
  AddAssign      -> Add
  SubAssign      -> Sub
  MulAssign      -> Mul
  DivAssign      -> Div
  FloorDivAssign -> FloorDiv
  ModAssign      -> Mod

augName :: AugOp -> String
augName op = opName (augBinOp op) ++ "="

opName :: BinOp -> String
opName op = case op of
  Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"; FloorDiv -> "//"
  Mod -> "%"; Pow -> "**"; Eq -> "=="; Neq -> "!="; Lt -> "<"
  Le -> "<="; Gt -> ">"; Ge -> ">="; And -> "and"; Or -> "or"

-- Expressoes ---------------------------------------------------------------

-- | Verifica a expressao contra o tipo esperado; o erro de tipo
-- incompativel sai na posicao p, a do comando ou da chamada. Lambda,
-- lista e condicional passam o tipo esperado para dentro, o que tipa os
-- parametros da lambda e a lista vazia.
check :: Pos -> Ty -> Expr -> TC ()
check p esperado e = do
  t <- shallow esperado
  case (e, t) of
    (ELambda lp ps body, TFun pts r)
      | length pts == length ps -> do
          duplicados lp ps
          withScope (Map.fromList (zip ps (map BVar pts))) (check p r body)
    (EList lp es, TList a) -> mapM_ (check lp a) es
    (ECond _ a c b, _) -> cond c >> check p t a >> check p t b
    _ -> infer e >>= subtype p t

infer :: Expr -> TC Ty
infer e = case e of
  EInt _ _   -> pure TInt
  EFloat _ _ -> pure TFloat
  EStr _ _   -> pure TStr
  EBool _ _  -> pure TBool
  ENone _    -> pure TNone
  EVar p n   -> variable p n
  ESelf p -> do
    ctx <- gets stFun
    param <- lookupName "self"
    case (ctx >>= fcClass, param) of
      (Just c, _)            -> pure (TClass c)
      (Nothing, Just (BVar t)) -> pure t   -- erro ja dado no parametro
      _ -> report p "'self' outside a method" >> fresh
  EBinOp p op a b -> do
    ta <- infer a
    tb <- infer b
    binop p (opName op) op ta tb
  EUnOp p op a -> infer a >>= unop p op
  ECall p f args -> call p f args
  EIndex p b i -> do
    tb <- infer b >>= shallow
    ti <- infer i
    r <- case tb of
      TList a -> pure a
      TStr    -> pure TStr
      TMeta _ -> do
        a <- fresh
        unify p tb (TList a)
        pure a
      _ -> do
        st <- showOne tb
        report p ("indexing requires 'list[T]' or 'str', got '" ++ st ++ "'")
        fresh
    indexType p ti
    pure r
  EField p o f -> do
    t <- infer o >>= shallow
    member p t f
  EList p es -> case es of
    [] -> TList <$> fresh
    x : xs -> do
      t0 <- infer x
      TList <$> foldM (\acc y -> infer y >>= join p acc) t0 xs
  ELambda p ps body -> do
    duplicados p ps
    ts <- mapM (const fresh) ps
    tb <- withScope (Map.fromList (zip ps (map BVar ts))) (infer body)
    pure (TFun ts tb)
  ECond p a c b -> do
    cond c
    ta <- infer a
    tb <- infer b
    join p ta tb

duplicados :: Pos -> [Name] -> TC ()
duplicados p ps = case [n | (i, n) <- zip [0 :: Int ..] ps, n `elem` take i ps] of
  n : _ -> report p ("duplicate parameter: '" ++ n ++ "'")
  []    -> pure ()

indexType :: Pos -> Ty -> TC ()
indexType p ti = do
  t <- shallow ti
  case t of
    TInt    -> pure ()
    TMeta _ -> unify p TInt t
    _ -> do
      st <- showOne t
      report p ("index must be 'int', got '" ++ st ++ "'")

variable :: Pos -> Name -> TC Ty
variable p n = do
  b <- lookupName n
  case b of
    Just (BVar t)  -> pure t
    Just (BFun sc) -> instantiate sc
    Just BClass    -> constructor n
    Nothing -> case builtinValue n of
      Just sc -> instantiate sc
      Nothing
        | n `elem` ["int", "float", "str"] -> do
            report p ("builtin function '" ++ n ++ "' can only be called")
            fresh
        | otherwise -> report p ("undeclared variable: '" ++ n ++ "'") >> fresh

-- | Tipo do nome da classe usado como funcao: os parametros do
-- __init__, proprio ou herdado.
constructor :: Name -> TC Ty
constructor c = do
  ini <- lookupMethod c "__init__"
  pure (TFun (maybe [] (map snd . mParams) ini) (TClass c))

-- | Campo de um objeto do tipo t (regra 8).
fieldOf :: Pos -> Ty -> Name -> TC (Maybe Ty)
fieldOf p t f = case t of
  TClass c -> do
    mf <- lookupField c f
    case mf of
      Just ft -> pure (Just ft)
      Nothing -> Nothing <$ report p ("class '" ++ c ++ "' has no field '" ++ f ++ "'")
  -- objeto de tipo desconhecido vem de um erro ja reportado
  TMeta _ -> pure Nothing
  _ -> do
    st <- showOne t
    Nothing <$ report p ("type '" ++ st ++ "' has no field '" ++ f ++ "'")

-- | obj.f fora de chamada: campo, ou metodo ja ligado ao objeto.
member :: Pos -> Ty -> Name -> TC Ty
member p t f = case t of
  TClass c -> do
    mf <- lookupField c f
    mm <- lookupMethod c f
    case (mf, mm) of
      (Just ft, _)       -> pure ft
      (Nothing, Just m)  -> pure (methodType m)
      (Nothing, Nothing) -> report p ("class '" ++ c ++ "' has no field '" ++ f ++ "'") >> fresh
  TList a | f == "append" -> pure (TFun [a] TNone)
  _ -> fieldOf p t f >>= maybe fresh pure

call :: Pos -> Expr -> [Expr] -> TC Ty
call p f args = case f of
  EVar _ n -> do
    b <- lookupName n
    case b of
      Nothing | n `elem` builtins -> builtin p n args
      _ -> infer f >>= apply p args
  EField fp o m -> methodCall p fp o m args
  _ -> infer f >>= apply p args

-- | obj.m(args), com despacho pelo tipo estatico (regra 17), e a chamada
-- qualificada Pai.m(self, args).
methodCall :: Pos -> Pos -> Expr -> Name -> [Expr] -> TC Ty
methodCall p fp o m args = do
  qualificada <- case o of
    EVar _ n -> do
      b <- lookupName n
      pure (case b of
        Just BClass -> Just n
        _           -> Nothing)
    _ -> pure Nothing
  case qualificada of
    Just c -> do
      mm <- lookupMethod c m
      case mm of
        Just mt -> apply p args (TFun (TClass c : map snd (mParams mt)) (mRet mt))
        Nothing -> semMetodo c
    Nothing -> do
      t <- infer o >>= shallow
      case t of
        TClass c -> do
          mf <- lookupField c m
          mm <- lookupMethod c m
          case (mf, mm) of
            (Just ft, _)       -> apply p args ft
            (Nothing, Just mt) -> apply p args (methodType mt)
            (Nothing, Nothing) -> semMetodo c
        TList a | m == "append" -> apply p args (TFun [a] TNone)
        TMeta _ -> mapM_ infer args >> fresh
        _ -> do
          st <- showOne t
          report fp ("type '" ++ st ++ "' has no method '" ++ m ++ "'")
          mapM_ infer args >> fresh
  where
    semMetodo c = do
      report fp ("class '" ++ c ++ "' has no method '" ++ m ++ "'")
      mapM_ infer args >> fresh

-- | Aplica uma funcao de tipo tf (regra 3). Os argumentos que nao sao
-- lambda vao primeiro: assim as incognitas dos parametros ja estao
-- resolvidas quando as lambdas sao tipadas, como em
-- fold(lambda acc, x: acc + x, 0, nums).
apply :: Pos -> [Expr] -> Ty -> TC Ty
apply p args tf = do
  t <- shallow tf
  case t of
    TFun ps r
      | length ps == length args -> do
          let (lambdas, outros) = partition (isLambda . snd) (zip ps args)
          forM_ (outros ++ lambdas) $ \(pt, a) -> check p pt a
          pure r
      | otherwise -> do
          ts <- mapM infer args
          r' <- fresh
          incompatible p t (TFun ts r')
          pure r
    TMeta _ -> do
      ts <- mapM infer args
      r <- fresh
      unify p t (TFun ts r)
      pure r
    _ -> do
      ts <- mapM infer args
      r <- fresh
      incompatible p t (TFun ts r)
      pure r
  where
    isLambda a = case a of
      ELambda {} -> True
      _          -> False

-- Primitivas ---------------------------------------------------------------

builtins :: [Name]
builtins = ["print", "input", "int", "float", "str", "bool", "len", "range", "append"]

-- | Tipo das primitivas usadas como valor. int, float e str aceitam
-- mais de um tipo de argumento e so podem ser chamadas.
builtinValue :: Name -> Maybe Scheme
builtinValue n = case n of
  "print"  -> Just (Forall [] (TFun [TStr] TNone))
  "input"  -> Just (Forall [] (TFun [] TStr))
  "bool"   -> Just (Forall [] (TFun [TInt] TBool))
  "range"  -> Just (Forall [] (TFun [TInt] (TList TInt)))
  "len"    -> Just (Forall ["t"] (TFun [TList (TGen "t")] TInt))
  "append" -> Just (Forall ["t"] (TFun [TList (TGen "t"), TGen "t"] TNone))
  _        -> Nothing

-- | Chamada de primitiva. len, append, int, float e str sao casos
-- especiais, porque aceitam tipos diferentes (secao 2.3 do enunciado).
builtin :: Pos -> Name -> [Expr] -> TC Ty
builtin p n args = case (n, args) of
  ("print", [a])  -> TNone <$ check p TStr a
  ("input", [])   -> pure TStr
  ("bool", [a])   -> TBool <$ check p TInt a
  ("int", [a])    -> TInt <$ conversao [TStr, TFloat] a
  ("float", [a])  -> TFloat <$ conversao [TStr, TInt] a
  ("str", [a])    -> TStr <$ conversao [TInt, TFloat, TBool] a
  ("len", [a]) -> do
    t <- infer a >>= shallow
    case t of
      TList _ -> pure ()
      TStr    -> pure ()
      TMeta _ -> pure ()
      _       -> espera "'list[T]' or 'str'" t
    pure TInt
  ("range", _) | length args `elem` [1, 2, 3] -> TList TInt <$ mapM_ (check p TInt) args
  ("append", [xs, x]) -> do
    t <- infer xs >>= shallow
    case t of
      TList a -> check p a x
      TMeta _ -> do
        a <- fresh
        unify p t (TList a)
        check p a x
      _ -> espera "'list[T]'" t >> void (infer x)
    pure TNone
  _ -> do
    mapM_ infer args
    report p ("function '" ++ n ++ "' expects " ++ aridade ++ ", got " ++ show (length args))
    pure resultado
  where
    conversao tipos a = do
      t <- infer a >>= shallow
      case t of
        TMeta _ -> pure ()
        _ | t `elem` tipos -> pure ()
          | otherwise      -> espera (alternativas tipos) t
    espera oque t = do
      st <- showOne t
      report p ("function '" ++ n ++ "' expects " ++ oque ++ ", got '" ++ st ++ "'")
    alternativas tipos = case reverse (map (\t -> "'" ++ showTy t ++ "'") tipos) of
      ultimo : resto@(_ : _) -> intercalate ", " (reverse resto) ++ " or " ++ ultimo
      ts                     -> concat ts
    (aridade, resultado) = case n of
      "print"  -> ("1 argument", TNone)
      "input"  -> ("0 arguments", TStr)
      "range"  -> ("1 to 3 arguments", TList TInt)
      "append" -> ("2 arguments", TNone)
      "len"    -> ("1 argument", TInt)
      "int"    -> ("1 argument", TInt)
      "float"  -> ("1 argument", TFloat)
      "str"    -> ("1 argument", TStr)
      _        -> ("1 argument", TBool)

-- Operadores ---------------------------------------------------------------

-- | Regras dos operadores (secoes 6.3 a 6.5 e regras 1 e 10 da secao 10).
-- Com erro, o resultado e o tipo que o operador produziria, para a
-- verificacao do comando seguir.
binop :: Pos -> String -> BinOp -> Ty -> Ty -> TC Ty
binop p nome op ta tb = do
  a <- shallow ta
  b <- shallow tb
  case op of
    Add      -> mesmoTipo a b [TInt, TFloat, TStr] "(int or float)" id
    Sub      -> mesmoTipo a b [TInt, TFloat] "(int or float)" id
    Mul      -> mesmoTipo a b [TInt, TFloat] "(int or float)" id
    Pow      -> mesmoTipo a b [TInt, TFloat] "(int or float)" id
    Div      -> mesmoTipo a b [TInt, TFloat] "(int or float)" (const TFloat)
    FloorDiv -> operandos a b TInt
    Mod      -> operandos a b TInt
    And      -> operandos a b TBool
    Or       -> operandos a b TBool
    Lt       -> mesmoTipo a b [TInt, TFloat, TStr] "(int, float or str)" (const TBool)
    Le       -> mesmoTipo a b [TInt, TFloat, TStr] "(int, float or str)" (const TBool)
    Gt       -> mesmoTipo a b [TInt, TFloat, TStr] "(int, float or str)" (const TBool)
    Ge       -> mesmoTipo a b [TInt, TFloat, TStr] "(int, float or str)" (const TBool)
    Eq       -> igualdade a b
    Neq      -> igualdade a b
  where
    -- os dois operandos com o mesmo tipo, um dos permitidos
    mesmoTipo a b permitidos quais res = case (a, b) of
      _ | a == b && a `elem` permitidos -> pure (res a)
      (TMeta _, TMeta _) -> res a <$ unify p a b
      (TMeta _, _) | b `elem` permitidos -> res b <$ unify p a b
      (_, TMeta _) | a `elem` permitidos -> res a <$ unify p a b
      _ -> do
        (sa, sb) <- quoted a b
        report p ("operator '" ++ nome ++ "' requires operands of the same type "
                  ++ quais ++ ", got '" ++ sa ++ "' and '" ++ sb ++ "'")
        pure (res a)
    -- os dois operandos do tipo alvo: int para // e %, bool para and e or
    operandos a b alvo
      | serve a && serve b = alvo <$ (unify p alvo a >> unify p alvo b)
      | otherwise = do
          (sa, sb) <- quoted a b
          report p ("operator '" ++ nome ++ "' requires '" ++ showTy alvo
                    ++ "' operands, got '" ++ sa ++ "' and '" ++ sb ++ "'")
          pure alvo
      where
        serve t = case t of
          TMeta _ -> True
          _       -> t == alvo
    igualdade a b = do
      ok <- case (a, b) of
        (TClass c, TClass d) -> (||) <$> subclass c d <*> subclass d c
        _                    -> tryUnify a b
      unless ok $ do
        (sa, sb) <- quoted a b
        report p ("operator '" ++ nome ++ "' requires operands of the same type, got '"
                  ++ sa ++ "' and '" ++ sb ++ "'")
      pure TBool

unop :: Pos -> UnOp -> Ty -> TC Ty
unop p op ta = do
  a <- shallow ta
  case (op, a) of
    (Neg, TInt)    -> pure TInt
    (Neg, TFloat)  -> pure TFloat
    (Neg, TMeta _) -> pure a
    (Neg, _) -> do
      st <- showOne a
      report p ("operator '-' requires an 'int' or 'float' operand, got '" ++ st ++ "'")
      pure a
    (Not, TBool)   -> pure TBool
    (Not, TMeta _) -> TBool <$ unify p TBool a
    (Not, _) -> do
      st <- showOne a
      report p ("operator 'not' requires a 'bool' operand, got '" ++ st ++ "'")
      pure TBool
