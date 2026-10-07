module Main (main) where

import MiniPython.CLI (Mode (..), Options (..), parseArgs, usage)
import MiniPython.Frontend.Lexer.Indent (layout)
import MiniPython.Frontend.Lexer.MiniPythonLexer (formatLexError, renderToken, scan)
import MiniPython.Frontend.Lexer.Token (Token)
import MiniPython.Frontend.Parser.Core (formatParseError)
import MiniPython.Frontend.Parser.Happy.Grammar (parseProgramLALR)
import MiniPython.Frontend.Parser.PEG (formatPegError, parseProgramPEG)
import MiniPython.Frontend.Parser.RecursiveDescent (parseProgram)
import MiniPython.Frontend.Pretty.MiniPythonPretty (pretty)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
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
      Right ast -> putStrLn (pretty ast)
    ParsePEG -> case parseProgramPEG file toks of
      Left bundle -> abort (formatPegError file bundle)
      Right ast   -> putStrLn (pretty ast)
    ParseLALR -> case parseProgramLALR toks of
      Left msg  -> abort (file ++ ":" ++ msg)
      Right ast -> putStrLn (pretty ast)

-- | Executa o analisador lexico com a camada de indentacao, abortando
-- com mensagem posicionada em caso de erro.
tokenize :: FilePath -> String -> IO [Token]
tokenize file src = case scan src >>= layout of
  Left err   -> abort (formatLexError file err)
  Right toks -> pure toks

abort :: String -> IO a
abort msg = hPutStrLn stderr msg >> exitFailure
