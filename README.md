# Compilador MiniPython

Trabalhos Práticos 1 e 2 de BCC328 - Construção de Compiladores I
(DECOM/UFOP): compilador para o MiniPython, um subconjunto de Python 3
com anotações de tipo, classes e funções anônimas, em que os blocos são
delimitados por indentação. A linguagem está descrita em
[`spec/minipython.md`](spec/minipython.md).

Brenda Gabrielle Alves Nascimento, 24.1.4011

O executável `mpyc` tem um analisador léxico e três analisadores
sintáticos, que produzem a mesma árvore sintática abstrata (AST):

- descendente recursivo, escrito com Megaparsec;
- PEG, escrito com a biblioteca typed-peg;
- LALR, gerado pelo Happy.

No TP2 entraram o verificador de tipos e um interpretador que executa a
AST de programas bem tipados.

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
  --check           Executa a verificação de tipos e reporta erros semânticos.
  --run             Verifica tipos e interpreta o programa.
  --help            Exibe esta ajuda.
```

Sem opção, vale `--parse-rd`. Pelo cabal, os argumentos vão depois de
`--`:

```sh
cabal run mpyc -- --lex exemplos/fib.mpy
cabal run mpyc -- --parse-peg exemplos/busca.mpy
cabal run mpyc -- --check exemplos/fib.mpy
cabal run mpyc -- --run exemplos/mdc.mpy < exemplos/mdc.in
```

`--check` não imprime nada quando o programa está bem tipado. `--run`
só executa o programa se a verificação passar; a entrada e a saída do
programa são a entrada e a saída padrão.

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

Os erros de tipo saem no mesmo formato. O verificador não para no
primeiro erro: todos vão para a saída de erro, seguidos de uma linha em
branco, como nos arquivos `.err` de `exemplos/invalid/semantic/` que
vieram com o enunciado. Operador, chamada e indexação são apontados na
posição do operador, do `(` ou do `[`:

```
e01_int_mais_float.mpy:4:18: error: operator '+' requires operands of the same type (int or float), got 'int' and 'float'
e01_int_mais_float.mpy:4:5: error: incompatible types: 'float' and 'int'
```

Erro durante a execução (índice fora da lista, divisão por zero,
entrada que não é número) também sai assim, com a posição da expressão:

```
x01_indice_fora.mpy:5:19: error: list index out of range: index 3, length 3
```

## Verificação de tipos e interpretação

O verificador (`src/MiniPython/Semantic/`) trabalha por unificação: o
que não tem anotação (parâmetro de lambda, lista vazia, função sem
anotação) recebe uma incógnita, e as variáveis de tipo de uma função
polimórfica, como o `a` de `def identidade(x: a) -> a`, viram incógnitas
novas em cada chamada. A subtipagem entre classes entra onde um valor é
usado no lugar de outro: declaração, atribuição, argumento e retorno.
Além das dez regras da seção 2.3 do enunciado, ele confere a regra 13
da especificação (o `__init__` atribui todos os campos) e `self` fora
de método.

Algumas decisões onde a especificação não diz nada:

- cada bloco (`if`, `while`, `for`) abre um escopo, como pede o
  enunciado; a variável declarada dentro dele não existe depois;
- `x = e` declara `x` quando o nome ainda não existe; dentro de função
  as globais podem ser lidas, mas `x = e` cria uma local, como no Python
  sem `global`, e `x += e` numa global é erro;
- `+` também concatena `str`, como nos exemplos da especificação;
- `==` e `!=` aceitam dois valores do mesmo tipo, e objetos são
  comparados por identidade.

O interpretador (`src/MiniPython/Interpreter/`) guarda as variáveis em
escopos com referências mutáveis (`IORef`), as listas em uma referência
para `Data.Sequence` e os objetos com um mapa de campos. O método é
procurado a partir da classe concreta do objeto, o que dá o despacho
dinâmico; `Pai.__init__(self, ...)` procura a partir da classe escrita.
`str` de um `float` segue o `repr` do Python (`0.1 + 0.2` dá
`0.30000000000000004`, `1e16` dá `1e+16`).

## Testes

```sh
cabal test             # parsers, verificador e interpretador
bash testes/rodar.sh   # o mesmo, chamando o executável
```

Os programas de teste ficam em `exemplos/`:

- `exemplos/*.mpy`: programas do repositório;
- `exemplos/valid/` e `exemplos/invalid/`: os fornecidos com o enunciado,
  mais os de erro de tipo `e16` a `e30` e os de erro de execução em
  `invalid/runtime/`;
- `exemplos/especificacao/`: os 15 programas da seção 9 da especificação.

Para todo programa os três parsers têm que produzir a mesma AST,
posições incluídas. O resto depende dos arquivos ao lado:

- `.err` em `invalid/semantic/`: a saída de `--check` tem que ser
  exatamente o arquivo;
- `.err` em `invalid/runtime/`: o programa passa no verificador e
  `--run` termina com esse erro;
- `.err` nos outros diretórios: erro léxico ou sintático, e a saída de
  cada parser tem que ser o arquivo;
- sem `.err`: o programa tem que passar no verificador;
- `.out`: saída esperada de `--run`, com a entrada do `.in`, se houver.

As saídas dos programas válidos foram conferidas com o Python 3,
rodando os mesmos programas sem as anotações de tipo.

O arquivo do `e03_funcao_sem_return.err` veio vazio com o enunciado,
mas o TP2 pede que a falta de `return` seja detectada, então ele agora
tem a mensagem do verificador. Os outros `.err` do enunciado não foram
mudados.

Os casos da seção 5 do enunciado do TP2:

| Caso | Programa |
| --- | --- |
| MDC | `especificacao/secao_9_01_mdc.mpy` |
| Fibonacci | `especificacao/secao_9_02_fibonacci.mpy` |
| Busca linear | `especificacao/secao_9_03_busca_linear.mpy` |
| Ordenação por seleção | `especificacao/secao_9_04_selecao.mpy` |
| map e filter | `especificacao/secao_9_08_alta_ordem.mpy` |
| Formas com herança | `especificacao/secao_9_11_formas.mpy` |
| Pilha | `especificacao/secao_9_12_pilha.mpy` |
| Lambdas e closures | `especificacao/secao_9_15_lambdas.mpy` |
| Variável não declarada | `invalid/semantic/e05_variavel_nao_declarada.mpy` |
| `int + str` | `invalid/semantic/e16_int_mais_str.mpy` |
| Aridade errada | `invalid/semantic/e04_aridade_errada.mpy` |
| Falta de `return` | `invalid/semantic/e03_funcao_sem_return.mpy` |
| `break` e `continue` fora de laço | `invalid/semantic/e17_break_fora_laco.mpy` |
| Campo inexistente | `invalid/semantic/e18_campo_inexistente.mpy` |

`bash testes/gerar-saidas.sh` regenera `testes/saidas-esperadas.txt`,
com as árvores e as mensagens sintáticas, e `testes/saidas-tp2.txt`,
com código, entrada e saída esperada desses casos e as mensagens dos
demais testes de erro.

## Organização

```
app/Main.hs                      executável mpyc
src/MiniPython/CLI.hs            opções da linha de comando
src/MiniPython/Frontend/Lexer/   scanner, tokens e camada de indentação
src/MiniPython/Frontend/Parser/  os três parsers (o LALR em Happy/Grammar.y)
src/MiniPython/Frontend/Syntax/  AST e regras de contexto (return, break)
src/MiniPython/Frontend/Pretty/  impressão da AST
src/MiniPython/Semantic/         tipos e verificador de tipos
src/MiniPython/Interpreter/      valores e interpretador
src/MiniPython/Utils/Error.hs    formato das mensagens de erro
test/Main.hs                     suíte do cabal test
testes/                          scripts de teste e saídas esperadas
exemplos/                        programas de teste
docs/gramatica.md                GLC, gramática PEG e conflitos do LALR
report/relatorio.tex             relatório
spec/minipython.md               especificação da linguagem
```
