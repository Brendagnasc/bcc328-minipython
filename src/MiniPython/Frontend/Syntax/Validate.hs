-- | Regras de contexto que a GLC nao expressa, verificadas sobre a AST
-- depois da analise sintatica. Os tres parsers produzem a mesma AST e
-- passam por aqui, entao as mensagens sao as mesmas nos tres modos.
--
-- return so vale dentro de funcao, e break e continue so dentro de
-- laco (regra 8 da secao 10 da especificacao). O parametro repetido
-- tambem e regra de contexto, mas fica a cargo de cada parser, porque
-- o erro e apontado no ')' que fecha a lista, posicao que a AST nao
-- guarda.
module MiniPython.Frontend.Syntax.Validate
  ( validate
  , duplicateParam
  ) where

import MiniPython.Frontend.Syntax.MiniPythonSyntax
import MiniPython.Utils.Error

-- | Devolve o programa intacto ou o primeiro erro, na ordem do fonte.
validate :: Program -> Either Error Program
validate prog@(Program ds) = prog <$ mapM_ decl ds
  where
    decl (DFun fd)   = function fd
    decl (DClass cd) = mapM_ function (clsMethods cd)
    decl (DStmt s)   = stmt False False s
    function fd = mapM_ (stmt True False) (fnBody fd)

-- | Verifica um comando sabendo se ele esta dentro de uma funcao e
-- dentro de um laco.
stmt :: Bool -> Bool -> Stmt -> Either Error ()
stmt inFun inLoop s = case s of
  SReturn p _ | not inFun   -> Left (Error p "'return' outside a function")
  SBreak p    | not inLoop  -> Left (Error p "'break' outside a loop")
  SContinue p | not inLoop  -> Left (Error p "'continue' outside a loop")
  SIf _ _ thenB elifs elseB ->
    mapM_ (block inLoop) (thenB : [b | (_, _, b) <- elifs] ++ [elseB])
  SWhile _ _ body   -> block True body
  SFor _ _ _ body   -> block True body
  _                 -> Right ()
  where
    block loop = mapM_ (stmt inFun loop)

-- | Primeiro nome que se repete numa lista de parametros.
duplicateParam :: [Param] -> Maybe Name
duplicateParam = go [] . map paramName
  where
    go _ [] = Nothing
    go seen (n : ns)
      | n `elem` seen = Just n
      | otherwise     = go (n : seen) ns
