# Compilador MiniPython

Trabalho Prático 1 de BCC328 - Construção de Compiladores I (DECOM/UFOP):
análise léxica e sintática do MiniPython, um subconjunto de Python 3 com
anotações de tipo, classes e funções anônimas, em que os blocos são
delimitados por indentação. A linguagem está descrita em
[`spec/minipython.md`](spec/minipython.md).

Brenda Gabrielle Alves Nascimento, 24.1.4011

O executável `mpyc` tem um analisador léxico e três analisadores
sintáticos, que produzem a mesma árvore sintática abstrata (AST):

- descendente recursivo, escrito com Megaparsec;
- PEG, escrito com a biblioteca typed-peg;
- LALR, gerado pelo Happy.

## Compilação

É preciso ter GHC 9.6 e cabal; o Happy é instalado pelo próprio cabal,
como ferramenta de build. A typed-peg (versão 0.6) vem do Hackage, então
no primeiro build pode ser necessário atualizar o índice de pacotes:

```sh
cabal update
cabal build
```

Com Nix, `nix develop` abre um ambiente com as versões fixadas no
`flake.nix`.

## Uso

```
mpyc [OPÇÃO] <arquivo.mpy>

Opções:
  --lex             Executa somente o analisador léxico e imprime a lista de tokens.
  --parse-rd        Executa o analisador descendente recursivo e imprime a AST.
  --parse-peg       Executa o analisador PEG e imprime a AST.
  --parse-lalr      Executa o analisador LALR e imprime a AST.
  --help            Exibe esta ajuda.
```

Sem opção, vale `--parse-rd`. Pelo cabal, os argumentos vão depois de
`--`:

```sh
cabal run mpyc -- --lex exemplos/fib.mpy
cabal run mpyc -- --parse-peg exemplos/busca.mpy
```

A saída de `--lex` tem um token por linha: linha e coluna do primeiro
caractere, seguidas do construtor do token. O `EOF` sai sempre como
`0:0`.

```
1:1 TkDef
1:5 TkIdent "fib"
1:8 TkLParen
1:9 TkIdent "n"
...
```

As opções `--parse-*` imprimem a AST com `drawTree`, de `Data.Tree`.
Para `exemplos/mdc.mpy`, o começo da saída é:

```
Program
|
+- FunDef mdc(a: int, b: int) -> int
|  |
|  +- SWhile
|  |  |
|  |  +- EBinOp Neq
...
```

Erros léxicos e sintáticos vão para a saída de erro, no formato
`arquivo:linha:coluna: error: mensagem`, com o nome do arquivo sem o
diretório, e o programa termina com código 1:

```
s01_falta_dois_pontos_if.mpy:3:13: error: expected ':', got NEWLINE
l04_char_invalido.mpy:2:16: error: caractere inválido: '@'
```

## Testes

```sh
cabal test             # ASTs dos três parsers e mensagens dos .err
bash testes/rodar.sh   # o mesmo, chamando o executável
```

Os programas de teste ficam em `exemplos/`:

- `exemplos/*.mpy`: programas do repositório;
- `exemplos/valid/` e `exemplos/invalid/`: os fornecidos com o enunciado;
- `exemplos/especificacao/`: os 15 programas da seção 9 da especificação.

Um programa com arquivo `.err` ao lado é inválido, e a saída de erro de
cada parser tem que ser exatamente o conteúdo desse arquivo. Os demais
são válidos, e os três parsers têm que produzir a mesma AST, posições
incluídas. Os de `exemplos/invalid/semantic/` só têm erros de tipo, que
ficam para o próximo trabalho, e aqui contam como válidos.

`bash testes/gerar-saidas.sh` regenera `testes/saidas-esperadas.txt`,
com as árvores e as mensagens dos programas do repositório.

## Organização

```
app/Main.hs                      executável mpyc
src/MiniPython/CLI.hs            opções da linha de comando
src/MiniPython/Frontend/Lexer/   scanner, tokens e camada de indentação
src/MiniPython/Frontend/Parser/  os três parsers (o LALR em Happy/Grammar.y)
src/MiniPython/Frontend/Syntax/  AST e regras de contexto (return, break)
src/MiniPython/Frontend/Pretty/  impressão da AST
src/MiniPython/Utils/Error.hs    formato das mensagens de erro
test/Main.hs                     suíte do cabal test
testes/                          scripts de teste e saídas esperadas
exemplos/                        programas de teste
docs/gramatica.md                GLC, gramática PEG e conflitos do LALR
report/relatorio.tex             relatório
spec/minipython.md               especificação da linguagem
```
