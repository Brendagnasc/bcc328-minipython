module MiniPython.CLI
  ( Mode (..)
  , Options (..)
  , parseArgs
  , usage
  ) where

-- | Modos de operacao do compilador, um para cada opcao da linha de comando.
data Mode
  = Lex
  | ParseRD
  | ParsePEG
  | ParseLALR
  | Check
  | Interpret
  deriving (Eq, Show)

-- | Resultado da analise dos argumentos.
data Options
  = Run Mode FilePath
  | ShowHelp
  | BadUsage String
  deriving (Eq, Show)

-- | Analisa os argumentos da linha de comando.
-- Sem opcao explicita, o padrao e o parser descendente recursivo.
parseArgs :: [String] -> Options
parseArgs args = case args of
  ["--help"]   -> ShowHelp
  []           -> BadUsage "nenhum arquivo de entrada informado"
  [file]       -> dispatch ParseRD file
  [flag, file] -> case modeOf flag of
                    Just m  -> dispatch m file
                    Nothing -> BadUsage ("opcao desconhecida: " ++ flag)
  _            -> BadUsage "argumentos em excesso"

-- | Associa cada flag ao seu modo.
modeOf :: String -> Maybe Mode
modeOf "--lex"        = Just Lex
modeOf "--parse-rd"   = Just ParseRD
modeOf "--parse-peg"  = Just ParsePEG
modeOf "--parse-lalr" = Just ParseLALR
modeOf "--check"      = Just Check
modeOf "--run"        = Just Interpret
modeOf _              = Nothing

-- | Valida a entrada antes de aceitar a execucao.
dispatch :: Mode -> FilePath -> Options
dispatch m file
  | take 1 file == "-" = BadUsage ("opcao desconhecida: " ++ file)
  | otherwise          = Run m file

-- | Texto da secao 6 do enunciado do TP1, com as opcoes do TP2.
usage :: String
usage = unlines
  [ "mpyc [OPÇÃO] <arquivo.mpy>"
  , ""
  , "Opções:"
  , "  --lex             Executa somente o analisador léxico e imprime a lista de tokens."
  , "  --parse-rd        Executa o analisador descendente recursivo e imprime a AST."
  , "  --parse-peg       Executa o analisador PEG e imprime a AST."
  , "  --parse-lalr      Executa o analisador LALR e imprime a AST."
  , "  --check           Executa a verificação de tipos e reporta erros semânticos."
  , "  --run             Verifica tipos e interpreta o programa."
  , "  --help            Exibe esta ajuda."
  , ""
  , "Se nenhuma opção for fornecida, o comportamento padrão é --parse-rd."
  ]
