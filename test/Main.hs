-- | Testes sobre os programas de exemplos/.
--
-- Os tres parsers tem que dar o mesmo resultado para todo programa,
-- posicoes incluidas, o que a arvore impressa nao mostra. O resto
-- depende dos arquivos ao lado do programa:
--
-- * .err em invalid/semantic: erros de tipo; a saida do verificador
--   tem que ser exatamente a do arquivo;
-- * .err em invalid/runtime: o programa esta bem tipado e a execucao
--   termina com o erro do arquivo;
-- * .err nos demais diretorios: erro lexico ou sintatico, e a mensagem
--   dos tres parsers tem que ser a do arquivo, como no TP1;
-- * sem .err: o programa tem que passar no verificador;
-- * .out: saida esperada da execucao, com a entrada do .in, se houver.
module Main (main) where

import Control.Monad (forM)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import MiniPython.Frontend.Lexer.Indent (layout)
import MiniPython.Frontend.Lexer.Token (Token)
import MiniPython.Frontend.Lexer.MiniPythonLexer (scan)
import MiniPython.Frontend.Parser.Happy.Grammar (parseProgramLALR)
import MiniPython.Frontend.Parser.PEG (parseProgramPEG)
import MiniPython.Frontend.Parser.RecursiveDescent (parseProgram)
import MiniPython.Frontend.Syntax.MiniPythonSyntax (Program)
import MiniPython.Frontend.Syntax.Validate (validate)
import MiniPython.Interpreter.Interp (Console (..), runProgram)
import MiniPython.Semantic.TypeChecker (typeCheck)
import MiniPython.Utils.Error (Error, formatError, formatErrors)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension, takeFileName, (</>))
import System.IO (hSetEncoding, stdout)

-- | Os tres parsers, com o mesmo encadeamento do executavel.
parsers :: [(String, String -> Either Error Program)]
parsers =
  [ ("--parse-rd",   \src -> tokens src >>= parseProgram >>= validate)
  , ("--parse-peg",  \src -> tokens src >>= parseProgramPEG src >>= validate)
  , ("--parse-lalr", \src -> tokens src >>= parseProgramLALR >>= validate)
  ]

tokens :: String -> Either Error [Token]
tokens src = scan src >>= layout

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  arquivos <- programas "exemplos"
  falhas <- concat <$> forM arquivos testar
  mapM_ putStrLn falhas
  putStrLn (show (length arquivos) ++ " programas, " ++ show (length falhas) ++ " falhas")
  if null falhas then pure () else exitFailure

-- | Todos os .mpy abaixo do diretorio, em ordem.
programas :: FilePath -> IO [FilePath]
programas dir = do
  nomes <- sort <$> listDirectory dir
  fmap concat . forM nomes $ \n -> do
    let caminho = dir </> n
    ehDir <- doesDirectoryExist caminho
    if ehDir
      then programas caminho
      else pure [caminho | ".mpy" `isSuffixOf` n]

lerSeExiste :: FilePath -> IO (Maybe String)
lerSeExiste f = do
  existe <- doesFileExist f
  if existe then Just <$> readFile f else pure Nothing

-- | Devolve as mensagens de falha do programa, vazia se passou.
testar :: FilePath -> IO [String]
testar arquivo = do
  src <- readFile arquivo
  err <- lerSeExiste (replaceExtension arquivo "err")
  out <- lerSeExiste (replaceExtension arquivo "out")
  entrada <- maybe [] lines <$> lerSeExiste (replaceExtension arquivo "in")
  let nome = takeFileName arquivo
      mensagem = either (formatError nome) (const "aceito")
      semantico = "invalid/semantic" `isInfixOf` arquivo
      execucao = "invalid/runtime" `isInfixOf` arquivo
      resultados = [(modo, parser src) | (modo, parser) <- parsers]
  if isJust err && not semantico && not execucao
    then pure [ arquivo ++ " (" ++ modo ++ "): " ++ mensagem r
              | (modo, r) <- resultados
              , Just (mensagem r ++ "\n") /= err ]
    else case map snd resultados of
      r : resto | any (/= r) resto -> pure [arquivo ++ ": os tres parsers diferem"]
      _ -> case tokens src >>= parseProgram of
        Left e -> pure [arquivo ++ ": " ++ formatError nome e]
        Right prog -> do
          let erros = case typeCheck prog of
                [] -> ""
                es -> formatErrors nome es
              esperado = if semantico then fromMaybe "" err else ""
          if erros /= esperado
            then pure [arquivo ++ " (--check): " ++ if null erros then "aceito" else erros]
            else if semantico || (not execucao && isNothing out)
              then pure []
              else executar nome prog entrada out (if execucao then fromMaybe "" err else "")
  where
    executar nome prog entrada out erroEsperado = do
      resto <- newIORef entrada
      saida <- newIORef []
      let ler = do
            ls <- readIORef resto
            case ls of
              []     -> pure Nothing
              l : ms -> Just l <$ writeIORef resto ms
          escrever l = modifyIORef saida (l :)
      r <- runProgram (Console ler escrever) prog
      obtida <- unlines . reverse <$> readIORef saida
      let erroObtido = either ((++ "\n") . formatError nome) (const "") r
      pure $ [arquivo ++ " (--run): saida diferente do .out" | maybe False (/= obtida) out]
          ++ [arquivo ++ " (--run): " ++ if null erroObtido then "sem erro" else erroObtido
             | erroObtido /= erroEsperado]
