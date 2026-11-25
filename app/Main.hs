module Main (main) where

import Data.Tree (drawTree)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import MiniPython.CLI (Mode (..), Options (..), parseArgs, usage)
import MiniPython.Frontend.Lexer.Indent (layout)
import MiniPython.Frontend.Lexer.MiniPythonLexer (renderToken, scan)
import MiniPython.Frontend.Parser.Happy.Grammar (parseProgramLALR)
import MiniPython.Frontend.Parser.PEG (parseProgramPEG)
import MiniPython.Frontend.Parser.RecursiveDescent (parseProgram)
import MiniPython.Frontend.Pretty.MiniPythonPretty (toTree)
import MiniPython.Frontend.Syntax.MiniPythonSyntax (Program)
import MiniPython.Frontend.Syntax.Validate (validate)
import MiniPython.Interpreter.Interp (runProgram, stdConsole)
import MiniPython.Semantic.TypeChecker (typeCheck)
import MiniPython.Utils.Error (Error, formatError, formatErrors)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName)
import System.IO (hFlush, hPutStr, hPutStrLn, hSetEncoding, stderr, stdin, stdout)

main :: IO ()
main = do
  -- Fonte e saidas sempre em UTF-8, qualquer que seja o locale: as
  -- mensagens de erro lexico tem acento, como nos arquivos .err.
  setLocaleEncoding utf8
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case parseArgs args of
    ShowHelp -> putStr usage >> exitSuccess
    BadUsage msg -> do
      hPutStrLn stderr ("erro: " ++ msg)
      hPutStr stderr usage
      exitFailure
    Run mode file -> do
      src <- readFile file
      run mode file src

run :: Mode -> FilePath -> String -> IO ()
run mode file src = do
  -- O lexer roda em todos os modos, inclusive no PEG, que le o texto
  -- direto: assim os erros lexicos saem iguais nos tres.
  toks <- orAbort file (scan src >>= layout)
  case mode of
    Lex       -> mapM_ (putStrLn . renderToken) toks
    ParseRD   -> printAst (parseProgram toks)
    ParsePEG  -> printAst (parseProgramPEG src toks)
    ParseLALR -> printAst (parseProgramLALR toks)
    Check     -> checked >> pure ()
    Interpret -> do
      prog <- checked
      r <- runProgram stdConsole prog
      -- o que o programa ja imprimiu sai antes da mensagem de erro
      hFlush stdout
      _ <- orAbort file r
      pure ()
  where
    printAst :: Either Error Program -> IO ()
    printAst result = do
      prog <- orAbort file (result >>= validate)
      putStrLn (drawTree (toTree prog))

    -- Verificacao de tipos sobre a AST do descendente recursivo. O
    -- verificador ja trata return, break e continue fora de lugar, entao
    -- validate nao e chamado. Com erro, todos vao para stderr e o
    -- programa nao e executado.
    checked :: IO Program
    checked = do
      prog <- orAbort file (scan src >>= layout >>= parseProgram)
      case typeCheck prog of
        [] -> pure prog
        errs -> do
          hPutStr stderr (formatErrors (takeFileName file) errs)
          exitFailure

-- | Imprime o erro em stderr e encerra. Como nos arquivos .err, a
-- mensagem leva so o nome do arquivo, sem o diretorio.
orAbort :: FilePath -> Either Error a -> IO a
orAbort file = either abort pure
  where
    abort e = hPutStrLn stderr (formatError (takeFileName file) e) >> exitFailure
