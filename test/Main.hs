-- | Testes do frontend sobre os programas de exemplos/.
--
-- Um programa acompanhado de .err e invalido: nos tres modos a mensagem
-- tem que ser exatamente a do arquivo, como na secao 7 do enunciado. Os
-- demais sao validos e os tres parsers tem que produzir a mesma AST,
-- posicoes incluidas, o que a arvore impressa nao mostra. Os programas
-- de invalid/semantic so tem erros de tipo, que sao do proximo
-- trabalho; aqui eles contam como validos.
module Main (main) where

import Control.Monad (forM)
import Data.List (isInfixOf, isSuffixOf, sort)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import MiniPython.Frontend.Lexer.Indent (layout)
import MiniPython.Frontend.Lexer.MiniPythonLexer (scan)
import MiniPython.Frontend.Parser.Happy.Grammar (parseProgramLALR)
import MiniPython.Frontend.Parser.PEG (parseProgramPEG)
import MiniPython.Frontend.Parser.RecursiveDescent (parseProgram)
import MiniPython.Frontend.Syntax.MiniPythonSyntax (Program)
import MiniPython.Frontend.Syntax.Validate (validate)
import MiniPython.Utils.Error (Error, formatError)
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
  where
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

-- | Devolve as mensagens de falha do programa, vazia se passou.
testar :: FilePath -> IO [String]
testar arquivo = do
  src <- readFile arquivo
  let errFile = replaceExtension arquivo "err"
  temErr <- doesFileExist errFile
  if temErr && not ("invalid/semantic" `isInfixOf` arquivo)
    then do
      esperado <- readFile errFile
      pure [ arquivo ++ " (" ++ modo ++ "): " ++ obtido
           | (modo, parser) <- parsers
           , let obtido = mensagem (parser src)
           , obtido ++ "\n" /= esperado ]
    else pure $ case map (\(_, parser) -> parser src) parsers of
      (Right a : resto)
        | all (== Right a) resto -> []
        | otherwise -> [arquivo ++ ": as ASTs dos tres parsers diferem"]
      resultados -> [arquivo ++ ": " ++ mensagem r | r <- resultados]
  where
    mensagem = either (formatError (takeFileName arquivo)) (const "aceito")
