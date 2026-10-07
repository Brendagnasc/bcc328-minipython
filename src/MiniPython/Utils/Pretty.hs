-- | Classe para impressao de estruturas do compilador.
--
-- Segue a convencao usada no material da disciplina: uma classe fina
-- sobre Text.PrettyPrint.HughesPJ, de modo que toda impressao do
-- compilador seja escrita a mao e nenhuma dependa de instancias de
-- Show geradas automaticamente.
module MiniPython.Utils.Pretty
  ( Pretty (..)
  , pretty
  , module Text.PrettyPrint.HughesPJ
  ) where

import Text.PrettyPrint.HughesPJ

class Pretty a where
  ppr :: a -> Doc

pretty :: Pretty a => a -> String
pretty = render . ppr
