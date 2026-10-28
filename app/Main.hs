module Main (main) where

import Data.Tree (drawTree)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import MiniPython.CLI (Mode (..), Options (..), parseArgs, usage)
import MiniPython.Frontend.Lexer.Indent (layout)
import MiniPython.Frontend.Lexer.MiniPythonLexer (renderToken, scan)
import MiniPython.Frontend.Lexer.Token (Token)
import MiniPython.Frontend.Parser.Core (formatParseError)
import MiniPython.Frontend.Parser.Happy.Grammar (parseProgramLALR)
import MiniPython.Frontend.Parser.PEG (formatPegError, parseProgramPEG)
import MiniPython.Frontend.Parser.RecursiveDescent (parseProgram)
import MiniPython.Frontend.Pretty.MiniPythonPretty (toTree)
import MiniPython.Frontend.Syntax.MiniPythonSyntax (Program)
import MiniPython.Frontend.Syntax.Validate (validate)
import MiniPython.Utils.Error (formatError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout)

main :: IO ()
main = do
  -- Fonte e saidas sempre em UTF-8, qualquer que seja o locale: as
  -- mensagens de erro lexico tem acento, como nos arquivos .err.
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case parseArgs args of
    ShowHelp -> putStr usage >> exitSuccess
    BadUsage msg -> do
      hPutStrLn stderr ("erro: " ++ msg)
      hPutStrLn stderr usage
      exitFailure
    Run mode file -> do
      src <- readFile file
      run mode file src

run :: Mode -> FilePath -> String -> IO ()
run mode file src = do
  toks <- tokenize file src
  case mode of
    Lex -> mapM_ (putStrLn . renderToken) toks
    ParseRD -> case parseProgram toks of
      Left err  -> abort (formatParseError file err)
      Right ast -> printAst ast
    ParsePEG -> case parseProgramPEG file toks of
      Left bundle -> abort (formatPegError file bundle)
      Right ast   -> printAst ast
    ParseLALR -> case parseProgramLALR toks of
      Left msg  -> abort (file ++ ":" ++ msg)
      Right ast -> printAst ast
  where
    printAst :: Program -> IO ()
    printAst ast = case validate ast of
      Left e     -> abort (formatError (takeFileName file) e)
      Right prog -> putStrLn (drawTree (toTree prog))

-- | Executa o analisador lexico com a camada de indentacao, abortando
-- com mensagem posicionada em caso de erro. Como nos arquivos .err, a
-- mensagem leva so o nome do arquivo, sem o diretorio.
tokenize :: FilePath -> String -> IO [Token]
tokenize file src = case scan src >>= layout of
  Left err   -> abort (formatError (takeFileName file) err)
  Right toks -> pure toks

abort :: String -> IO a
abort msg = hPutStrLn stderr msg >> exitFailure
