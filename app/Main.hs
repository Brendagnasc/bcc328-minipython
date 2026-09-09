module Main (main) where

import MiniPython.CLI (Mode (..), Options (..), parseArgs, usage)
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

-- | Despacha para a fase correspondente. Cada ramo sera preenchido
-- nas proximas etapas do trabalho.
run :: Mode -> FilePath -> String -> IO ()
run mode file src = do
  putStrLn ("modo: " ++ show mode)
  putStrLn ("arquivo: " ++ file)
  putStrLn ("linhas: " ++ show (length (lines src)))
