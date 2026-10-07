module Main (main) where

import MiniPython.CLI (Mode (..), Options (..), parseArgs, usage)
import MiniPython.Frontend.Lexer.MiniPythonLexer (formatLexError, renderToken, scan)
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
run mode file src = case mode of
  Lex -> case scan src of
    Left err   -> hPutStrLn stderr (formatLexError file err) >> exitFailure
    Right toks -> mapM_ (putStrLn . renderToken) toks
  _ -> putStrLn ("modo " ++ show mode ++ " ainda nao implementado")
