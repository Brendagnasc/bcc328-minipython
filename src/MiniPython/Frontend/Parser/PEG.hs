{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Analisador sintatico baseado em PEG, escrito com a typed-peg.
--
-- A typed-peg le caracteres, e nao tokens, entao esta gramatica cuida
-- tambem da parte lexica e da indentacao. A linguagem e a AST, com as
-- posicoes, sao as mesmas dos outros dois parsers; o porque das regras
-- de layout e dos %label esta em docs/gramatica.md.
--
-- Em resumo: um bloco e e^> com cada comando alinhado (|e|), como no
-- exemplo Layout da disciplina; a relacao entre tokens e gtR, de modo
-- que a quebra de linha so vira espaco dentro de parenteses e
-- colchetes, onde vale _~; e os %label com nome vazio escondem o espaco
-- e o primeiro token das continuacoes opcionais, para que a lista de
-- esperados de um erro tenha so o que de fato faltou.
--
-- As regras de comando devolvem Either Error porque tres restricoes so
-- podem ser verificadas na acao semantica, que nao faz o parse falhar:
-- alvo de atribuicao, parametro repetido e argumento de tipo.
module MiniPython.Frontend.Parser.PEG
  ( parseProgramPEG
  ) where

import Data.Char (isUpper)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import MiniPython.Frontend.Lexer.Token
import MiniPython.Frontend.Syntax.MiniPythonSyntax
import MiniPython.Frontend.Syntax.Validate (duplicateParam)
import MiniPython.Utils.Error
import PEG hiding (Name, Not)
import PEG.QQ (pegGrammar)

type R = Either Error

-- | Linhas do fonte indexadas pelo deslocamento do primeiro caractere.
-- A typed-peg entrega deslocamentos; a conversao para linha e coluna
-- segue o lexer, com tabulacao ate a proxima coluna multipla de 8.
newtype Source = Source (Map.Map Int (Int, String))

source :: String -> Source
source txt = Source (Map.fromList (zip starts (zip [1 ..] ls)))
  where
    ls = splitLines txt
    starts = scanl (\o l -> o + length l + 1) 0 ls
    splitLines s = case break (== '\n') s of
      (l, [])       -> [l]
      (l, _ : rest) -> l : splitLines rest

posAt :: Source -> Int -> Pos
posAt (Source m) off = case Map.lookupLE off m of
  Just (start, (ln, txt)) -> Pos ln (foldl step 1 (take (off - start) txt))
  Nothing                 -> initialPos
  where
    step c '\t' = ((c - 1) `div` 8 + 1) * 8 + 1
    step c _    = c + 1

-- Construcao da AST a partir das acoes ----------------------------------

mkProgram :: [R Decl] -> R Program
mkProgram ds = Program <$> sequence ds

mkFunDef :: Source -> Int -> Name -> ([R Param], Int) -> Maybe (R Type) -> R Block -> R FunDef
mkFunDef src p n (rps, fecha) ret body = do
  ps <- sequence rps
  case duplicateParam ps of
    Just d  -> Left (Error (posAt src fecha) ("duplicate parameter: '" ++ d ++ "'"))
    Nothing -> FunDef (posAt src p) n ps <$> sequence ret <*> body

mkParam :: Source -> Int -> Name -> Maybe (R Type) -> R Param
mkParam src p n t = Param (posAt src p) n <$> sequence t

mkClassDef :: Source -> Int -> Name -> Maybe Name -> R ([Field], [FunDef]) -> R ClassDef
mkClassDef src p n super body = do
  (fs, ms) <- body
  pure (ClassDef (posAt src p) n super fs ms)

mkMembers :: [R Field] -> [R FunDef] -> R ([Field], [FunDef])
mkMembers fs ms = (,) <$> sequence fs <*> sequence ms

mkField :: Source -> Int -> Name -> R Type -> R Field
mkField src p n t = Field (posAt src p) n <$> t

mkIf :: Source -> Int -> Expr -> R Block -> [(Int, Expr, R Block)] -> Maybe (R Block) -> R Stmt
mkIf src p c thenB elifs elseB =
  SIf (posAt src p) c <$> thenB <*> mapM elif elifs <*> fromMaybe (Right []) elseB
  where
    elif (q, ec, eb) = (,,) (posAt src q) ec <$> eb

mkVarDecl :: Source -> Int -> Name -> R Type -> Expr -> R Stmt
mkVarDecl src p n t e = (\ty -> SVarDecl (posAt src p) n ty e) <$> t

-- | Expressao seguida, ou nao, de '=' ou de um operador aumentado.
mkAssign :: Expr -> Maybe (Maybe AugOp, Expr) -> R Stmt
mkAssign e rest = case rest of
  Nothing              -> Right (SExpr (exprStart e) e)
  Just (Nothing, v)    -> (\lv -> SAssign (exprStart e) lv v) <$> toLValue e
  Just (Just op, v)    -> (\lv -> SAugAssign (exprStart e) lv op v) <$> toLValue e

toLValue :: Expr -> R LValue
toLValue e = case e of
  EVar p n     -> Right (LVar p n)
  EIndex p b i -> Right (LIndex p b i)
  EField p b n -> Right (LField p b n)
  _            -> Left (Error (exprStart e) "invalid assignment target")

mkFunType :: Source -> Int -> [R Type] -> R Type -> R Type
mkFunType src p ts r = TyFun (posAt src p) <$> sequence ts <*> r

-- | Nome de tipo, ou list[T]: so list recebe argumento entre colchetes.
mkNamedType :: Source -> Int -> Name -> Maybe (Int, R Type) -> R Type
mkNamedType src p n arg = case arg of
  Nothing -> Right (case n of
    (c : _) | isUpper c -> TyClass (posAt src p) n
    _                   -> TyVar (posAt src p) n)
  Just (q, rt) -> do
    t <- rt
    if n == "list"
      then Right (TyList (posAt src p) t)
      else Left (Error (posAt src q) ("unexpected '[' after type '" ++ n ++ "'"))

mkNumber :: Source -> Int -> String -> Maybe String -> Maybe String -> Expr
mkNumber src p i f x = case (f, x) of
  (Nothing, Nothing) -> EInt (posAt src p) (read texto)
  _                  -> EFloat (posAt src p) (read texto)
  where
    texto = filter (/= '_') (i ++ fromMaybe "" f ++ fromMaybe "" x)

mkCond :: Expr -> Maybe (Expr, Expr) -> Expr
mkCond e Nothing       = e
mkCond e (Just (c, a)) = ECond (exprStart e) e c a

-- Os operadores binarios e posfixos guardam o deslocamento do operador,
-- capturado com @ logo antes dele na gramatica.

mkRel :: Source -> Expr -> Maybe (Int, BinOp, Expr) -> Expr
mkRel _ e Nothing             = e
mkRel src e (Just (q, op, r)) = EBinOp (posAt src q) op e r

mkPow :: Source -> Expr -> Maybe (Int, Expr) -> Expr
mkPow _ b Nothing          = b
mkPow src b (Just (q, e))  = EBinOp (posAt src q) Pow b e

-- | Cadeia associativa a esquerda.
chain :: Source -> Expr -> [(Int, BinOp, Expr)] -> Expr
chain src = foldl (\acc (q, op, r) -> EBinOp (posAt src q) op acc r)

chainOf :: Source -> BinOp -> Expr -> [(Int, Expr)] -> Expr
chainOf src op e es = chain src e [(q, op, r) | (q, r) <- es]

data Postfix
  = PCall Int [Expr]
  | PIndex Int Expr
  | PField Int Name

mkPostfix :: Source -> Expr -> [Postfix] -> Expr
mkPostfix src = foldl step
  where
    step e (PCall q args) = ECall (posAt src q) e args
    step e (PIndex q i)   = EIndex (posAt src q) e i
    step e (PField q n)   = EField (posAt src q) e n

[pegGrammar|
  %name   miniPython
  %stream String
  %param  src :: Source
  %start  program

  %label  hs         ""
  %label  sp         ""
  %label  ind        ""
  %label  blanks     ""
  %label  nametail   ""
  %label  number     ""
  %label  string     ""
  %label  comma      ""
  %label  arrow      ""
  %label  colon      ""
  %label  superopen  ""
  %label  passkw     ""
  %label  elifkw     ""
  %label  elsekw     ""
  %label  assignop   ""
  %label  typeopen   ""
  %label  condkw     ""
  %label  orkw       ""
  %label  andkw      ""
  %label  relop      ""
  %label  addop      ""
  %label  mulop      ""
  %label  powkw      ""
  %label  lpar       ""
  %label  lbrk       ""
  %label  dot        ""
  %label  eol        "NEWLINE"
  %label  body       "INDENT (indented block)"
  %label  classbody  "INDENT (indented block)"
  %label  name       "identifier"
  %label  typ        "type"
  %label  stmt       "expression"
  %label  expr       "expression"
  %label  disj       "expression"
  %label  conj       "expression"
  %label  neg        "expression"
  %label  rel        "expression"
  %label  sum        "expression"
  %label  term       "expression"
  %label  power      "expression"
  %label  unary      "expression"
  %label  atom       "expression"

  program   :: R Program <- blanks ds:(ind d:|x:decl|)* end     { mkProgram ds }

  # O fim do arquivo tambem passa pela relacao de coluna: sob gtR o
  # primeiro caractere de uma linha nunca e aceito, e !. passaria no
  # meio do arquivo. Por isso o teste e feito com _~.
  end       :: ()        <- (hs !.)_~

  decl      :: R Decl    <- f:fundef    { fmap DFun f }
                          / c:classdef  { fmap DClass c }
                          / s:stmt      { fmap DStmt s }

  # Espaco, quebras de linha e indentacao.
  hs        :: ()        <- ([ \t\r] / '#' [^\n]*)*
  sp        :: ()        <- ([ \t\r] / '#' [^\n]* / &('\n' .) '\n')*
  eol       :: ()        <- (hs ('\n' / !.) blanks)_~
  blanks    :: ()        <- (hs '\n')*_~
  ind       :: ()        <- [ \t]*_~

  # Definicoes.
  fundef    :: R FunDef  <- p:@ "def" ![a-zA-Z0-9_] sp n:name '('
                            ps:(sp xs:params q:@ ')')_~ sp r:ret? ':' sp b:suite
                              { mkFunDef src p n ps r b }
  ret       :: R Type    <- arrow t:typ
  arrow     :: ()        <- "->" sp
  params    :: [R Param] <- q:param qs:(comma y:param)*  { q : qs }
                          / { [] }
  param     :: R Param   <- p:@ "self" ![a-zA-Z0-9_] sp  { Right (Param (posAt src p) "self" Nothing) }
                          / p:@ n:name t:annot?          { mkParam src p n t }
  annot     :: R Type    <- colon t:typ
  colon     :: ()        <- ':' sp

  classdef  :: R ClassDef <- p:@ "class" ![a-zA-Z0-9_] sp n:name s:super? ':' sp eol ind
                             b:classbody                 { mkClassDef src p n s b }
  super     :: Name      <- superopen n:name ')' sp
  superopen :: ()        <- '(' sp
  classbody :: R ([Field], [FunDef]) <- r:(m:members)^>
  members   :: R ([Field], [FunDef])
            <- f:|x:field| fs:(ind g:|y:field|)* ms:(ind h:|z:fundef|)* passtail?
                 { mkMembers (f : fs) ms }
             / h:|x:fundef| ms:(ind k:|z:fundef|)* passtail?
                 { mkMembers [] (h : ms) }
             / |"pass" ![a-zA-Z0-9_] sp| eol             { Right ([], []) }
  passtail  :: ()        <- passkw eol
  passkw    :: ()        <- ind |"pass" ![a-zA-Z0-9_] sp|
  field     :: R Field   <- p:@ n:name ':' sp t:typ eol  { mkField src p n t }

  # Blocos e comandos.
  suite     :: R Block   <- eol ind b:body
  body      :: R Block   <- r:(s:|x:stmt| ss:(ind t:|y:stmt|)* { sequence (s : ss) })^>

  stmt      :: R Stmt    <- f:fundef { fmap SFunDef f }
                          / a:ifstmt / b:whilestmt / c:forstmt / d:simple eol

  ifstmt    :: R Stmt    <- p:@ "if" ![a-zA-Z0-9_] sp c:expr ':' sp b:suite
                            es:elifcl* e:elsecl?       { mkIf src p c b es e }
  elifcl    :: (Int, Expr, R Block) <- q:elifkw c:expr ':' sp b:suite  { (q, c, b) }
  elifkw    :: Int       <- ind q:@ |"elif"| ![a-zA-Z0-9_] sp
  elsecl    :: R Block   <- elsekw ':' sp b:suite
  elsekw    :: ()        <- ind |"else"| ![a-zA-Z0-9_] sp

  whilestmt :: R Stmt    <- p:@ "while" ![a-zA-Z0-9_] sp c:expr ':' sp b:suite
                              { fmap (SWhile (posAt src p) c) b }
  forstmt   :: R Stmt    <- p:@ "for" ![a-zA-Z0-9_] sp v:name "in" ![a-zA-Z0-9_] sp
                            e:expr ':' sp b:suite      { fmap (SFor (posAt src p) v e) b }

  simple    :: R Stmt    <- p:@ "pass" ![a-zA-Z0-9_] sp      { Right (SPass (posAt src p)) }
                          / p:@ "break" ![a-zA-Z0-9_] sp     { Right (SBreak (posAt src p)) }
                          / p:@ "continue" ![a-zA-Z0-9_] sp  { Right (SContinue (posAt src p)) }
                          / p:@ "return" ![a-zA-Z0-9_] sp e:expr?
                              { Right (SReturn (posAt src p) e) }
                          / p:@ n:name ':' sp t:typ '=' !'=' sp e:expr
                              { mkVarDecl src p n t e }
                          / e:expr r:assigntail?         { mkAssign e r }
  assigntail :: (Maybe AugOp, Expr) <- o:assignop v:expr  { (o, v) }
  assignop  :: Maybe AugOp <- '=' !'=' sp   { Nothing }
                            / "+=" sp       { Just AddAssign }
                            / "-=" sp       { Just SubAssign }
                            / "*=" sp       { Just MulAssign }
                            / "//=" sp      { Just FloorDivAssign }
                            / "/=" sp       { Just DivAssign }
                            / "%=" sp       { Just ModAssign }

  # Tipos.
  typ       :: R Type    <- p:@ "int" ![a-zA-Z0-9_] sp    { Right (TyInt (posAt src p)) }
                          / p:@ "float" ![a-zA-Z0-9_] sp  { Right (TyFloat (posAt src p)) }
                          / p:@ "bool" ![a-zA-Z0-9_] sp   { Right (TyBool (posAt src p)) }
                          / p:@ "str" ![a-zA-Z0-9_] sp    { Right (TyStr (posAt src p)) }
                          / p:@ "None" ![a-zA-Z0-9_] sp   { Right (TyNone (posAt src p)) }
                          / p:@ '(' ts:(sp xs:types ')')_~ sp "->" sp r:typ
                              { mkFunType src p ts r }
                          / p:@ n:name a:typearg?         { mkNamedType src p n a }
  typearg   :: (Int, R Type) <- q:typeopen t:(sp x:typ ']')_~ sp  { (q, t) }
  typeopen  :: Int       <- q:@ '['
  types     :: [R Type]  <- t:typ ts:(comma u:typ)*  { t : ts }
                          / { [] }

  # Expressoes, da menor para a maior precedencia, como na GLC. A lambda
  # fica no nivel de expr, e nao entre os atomos: o corpo vai ate o fim.
  expr      :: Expr      <- p:@ "lambda" ![a-zA-Z0-9_] sp ps:lparams ':' sp b:expr
                              { ELambda (posAt src p) ps b }
                          / e:disj c:condtail?         { mkCond e c }
  condtail  :: (Expr, Expr) <- condkw c:disj "else" ![a-zA-Z0-9_] sp a:expr  { (c, a) }
  condkw    :: ()        <- "if" ![a-zA-Z0-9_] sp
  disj      :: Expr      <- e:conj es:(q:@ orkw r:conj { (q, r) })*  { chainOf src Or e es }
  orkw      :: ()        <- "or" ![a-zA-Z0-9_] sp
  conj      :: Expr      <- e:neg es:(q:@ andkw r:neg { (q, r) })*   { chainOf src And e es }
  andkw     :: ()        <- "and" ![a-zA-Z0-9_] sp
  neg       :: Expr      <- p:@ "not" ![a-zA-Z0-9_] sp e:neg  { EUnOp (posAt src p) Not e }
                          / e:rel
  rel       :: Expr      <- e:sum r:reltail?           { mkRel src e r }
  reltail   :: (Int, BinOp, Expr) <- q:@ o:relop r:sum  { (q, o, r) }
  relop     :: BinOp     <- "==" sp { Eq } / "!=" sp { Neq } / "<=" sp { Le }
                          / ">=" sp { Ge } / '<' sp { Lt } / '>' sp { Gt }
  sum       :: Expr      <- e:term es:(q:@ o:addop r:term { (q, o, r) })*  { chain src e es }
  addop     :: BinOp     <- '+' !'=' sp { Add } / '-' ![=>] sp { Sub }
  term      :: Expr      <- e:power es:(q:@ o:mulop r:power { (q, o, r) })*  { chain src e es }
  mulop     :: BinOp     <- "//" !'=' sp { FloorDiv } / '/' !'=' sp { Div }
                          / '%' !'=' sp { Mod } / '*' ![*=] sp { Mul }
  power     :: Expr      <- b:unary r:powtail?         { mkPow src b r }
  powtail   :: (Int, Expr) <- q:@ powkw e:power        { (q, e) }
  powkw     :: ()        <- "**" sp
  unary     :: Expr      <- p:@ '-' ![=>] sp e:unary   { EUnOp (posAt src p) Neg e }
                          / e:postfix
  postfix   :: Expr      <- a:atom ps:post*            { mkPostfix src a ps }
  post      :: Postfix   <- q:@ lpar args:(sp xs:exprs ')')_~ sp  { PCall q args }
                          / q:@ lbrk i:(sp x:expr ']')_~ sp       { PIndex q i }
                          / q:@ dot n:name                        { PField q n }
  lpar      :: ()        <- '('
  lbrk      :: ()        <- '['
  dot       :: ()        <- '.' sp

  atom      :: Expr      <- p:@ "True" ![a-zA-Z0-9_] sp   { EBool (posAt src p) True }
                          / p:@ "False" ![a-zA-Z0-9_] sp  { EBool (posAt src p) False }
                          / p:@ "None" ![a-zA-Z0-9_] sp   { ENone (posAt src p) }
                          / p:@ "self" ![a-zA-Z0-9_] sp   { ESelf (posAt src p) }
                          / p:@ k:convkw sp               { EVar (posAt src p) k }
                          / p:@ n:name                    { EVar (posAt src p) n }
                          / n:number
                          / p:@ s:string                  { EStr (posAt src p) s }
                          / p:@ '[' es:(sp xs:exprs ']')_~ sp  { EList (posAt src p) es }
                          / '(' e:(sp x:expr ')')_~ sp
  convkw    :: String    <- "int" ![a-zA-Z0-9_]   { "int" }
                          / "float" ![a-zA-Z0-9_] { "float" }
                          / "str" ![a-zA-Z0-9_]   { "str" }
                          / "bool" ![a-zA-Z0-9_]  { "bool" }
  exprs     :: [Expr]    <- e:expr es:(comma x:expr)*  { e : es }
                          / { [] }
  lparams   :: [Name]    <- n:name ns:(comma m:name)*  { n : ns }
                          / { [] }
  comma     :: ()        <- ',' sp

  # Lexico: nomes, palavras reservadas e literais. Em keyword, "int"
  # precisa vir antes de "in": a escolha e ordenada e nao volta atras.
  name      :: Name      <- !keyword c:[a-zA-Z_] cs:nametail sp  { c : cs }
  nametail  :: String    <- cs:[a-zA-Z0-9_]*
  keyword   :: ()        <- ( "and" / "break" / "bool" / "class" / "continue" / "def"
                            / "elif" / "else" / "False" / "float" / "for" / "if" / "int"
                            / "in" / "lambda" / "None" / "not" / "or" / "pass" / "return"
                            / "self" / "str" / "True" / "while" ) ![a-zA-Z0-9_]

  # O separador _ so vale entre dois digitos, como no lexer.
  number    :: Expr      <- p:@ i:digits f:fraction? x:exponent? sp  { mkNumber src p i f x }
  digits    :: String    <- d:[0-9] ds:digitmore*      { d : ds }
  digitmore :: Char      <- '_'? d:[0-9]
  fraction  :: String    <- '.' ds:digits              { '.' : ds }
  exponent  :: String    <- e:[eE] s:sign ds:digits    { e : s ++ ds }
  sign      :: String    <- '+' { "+" } / '-' { "-" } / { "" }

  string    :: String    <- '"' cs:dqchar* '"' sp      { cs }
                          / '\'' cs:sqchar* '\'' sp    { cs }
  dqchar    :: Char      <- '\\' c:escape { c } / c:[^"\\\n] { c }
  sqchar    :: Char      <- '\\' c:escape { c } / c:[^'\\\n] { c }
  escape    :: Char      <- 'n' { '\n' } / 't' { '\t' } / 'r' { '\r' }
                          / '\\' { '\\' } / '"' { '"' } / '\'' { '\'' }
|]

-- | Tokens em gtR (ver o cabecalho do modulo); os comandos de nivel
-- superior comecam na coluna 0.
opts :: Opts
opts = defaultOpts { optTokenMode = relD gtR, optCands = singletonI 0 }

-- | Reconhece um programa a partir do texto fonte. A lista de tokens do
-- lexer so e usada para descrever o token encontrado num erro.
parseProgramPEG :: String -> [Token] -> Either Error Program
parseProgramPEG txt toks = case parseWith opts (miniPython src) txt of
  OK r _ _ -> r
  Fail e   -> Left (pegError src toks e)
  where
    src = source txt

-- | Converte o erro da typed-peg para o formato dos outros parsers.
pegError :: Source -> [Token] -> ParseError -> Error
pegError src toks e = Error p (case esperado of
  Just w  -> "expected " ++ w ++ ", got " ++ achado
  Nothing -> "unexpected " ++ achado)
  where
    p = posAt src (errOffset e)
    achado = case [t | t <- toks, tokPos t == p] of
      (t : _) -> describeLexeme (tokLexeme t)
      []      -> case errFound e of
        FoundToken "\n" -> "NEWLINE"
        FoundToken w    -> "'" ++ w ++ "'"
        FoundEnd        -> "EOF"
    -- Um nome de regra diz mais que um caractere solto; sem nenhum,
    -- vale o primeiro item na ordem em que a gramatica tentou.
    esperado = case [l | ExpLabel l <- errExpected e] of
      (l : _) -> Just l
      []      -> case errExpected e of
        (x : _) -> Just (describe x)
        []      -> Nothing
    describe x = case x of
      ExpChar '\n' -> "NEWLINE"
      ExpChar c    -> "'" ++ [c] ++ "'"
      ExpString s  -> "'" ++ s ++ "'"
      ExpClass d   -> d
      ExpAny       -> "any character"
      ExpEnd       -> "EOF"
      ExpLabel l   -> l
