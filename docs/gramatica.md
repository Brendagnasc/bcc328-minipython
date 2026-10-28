# Gramática Livre de Contexto do MiniPython

Brenda Gabrielle Alves Nascimento, 24.1.4011
BCC328 - Construção de Compiladores I - Trabalho Prático 1

## Notação

Notação EBNF. Terminais em MAIÚSCULAS são tokens produzidos pelo
analisador léxico; terminais entre aspas são o lexema literal.
`{ X }` indica zero ou mais repetições de X e `[ X ]` indica X opcional.

Os tokens NEWLINE, INDENT e DEDENT são gerados pela camada de
indentação do analisador léxico (seção 4.5 da especificação) e
delimitam blocos como as chaves de linguagens baseadas em C.

## Estrutura do programa

    Programa     ::= { Decl } EOF

    Decl         ::= DefFuncao
                   | DefClasse
                   | Comando

    DefFuncao    ::= 'def' IDENT '(' [ Params ] ')' [ '->' Tipo ] ':' Bloco

    Params       ::= Param { ',' Param }
    Param        ::= 'self' | IDENT [ ':' Tipo ]

    DefClasse    ::= 'class' IDENT [ '(' IDENT ')' ] ':' BlocoClasse

    BlocoClasse  ::= NEWLINE INDENT { Campo } { DefFuncao }
                     [ 'pass' NEWLINE ] DEDENT

    Campo        ::= IDENT ':' Tipo NEWLINE

    Bloco        ::= NEWLINE INDENT Comando { Comando } DEDENT

`self` é palavra reservada (seção 4.2), por isso aparece à parte em
`Param`. Os campos de uma classe precedem os métodos, conforme a seção
6.3 da especificação. Uma classe sem campos e sem métodos tem como
corpo o comando `pass`, que não vai para a AST.

## Tipos

    Tipo         ::= TipoFuncao | TipoAtomico

    TipoFuncao   ::= '(' [ Tipo { ',' Tipo } ] ')' '->' Tipo

    TipoAtomico  ::= 'int' | 'float' | 'bool' | 'str' | 'None'
                   | IDENT [ '[' Tipo ']' ]

`list` não é palavra reservada, por isso o tipo lista é um IDENT
seguido do tipo dos elementos entre colchetes. Só `list` aceita esse
argumento: em `Foo[int]` o erro é apontado no `[`. Sem colchetes, o
IDENT é resolvido pelo analisador sintático: identificadores iniciados
por letra maiúscula são tipos de classe; os demais são variáveis de
tipo (seção 3.5 da especificação).

## Comandos

    Comando      ::= ComandoSimples NEWLINE
                   | ComandoComposto

    ComandoSimples
                 ::= 'pass'
                   | 'break'
                   | 'continue'
                   | 'return' [ Expr ]
                   | Atribuicao
                   | Expr

    Atribuicao   ::= IDENT ':' Tipo '=' Expr
                   | Alvo '=' Expr
                   | Alvo OpAumentado Expr

    Alvo         ::= IDENT
                   | ExprPosfixa '[' Expr ']'
                   | ExprPosfixa '.' IDENT

    OpAumentado  ::= '+=' | '-=' | '*=' | '/=' | '//=' | '%='

    ComandoComposto
                 ::= ComandoIf
                   | ComandoWhile
                   | ComandoFor

    ComandoIf    ::= 'if' Expr ':' Bloco
                     { 'elif' Expr ':' Bloco }
                     [ 'else' ':' Bloco ]

    ComandoWhile ::= 'while' Expr ':' Bloco

    ComandoFor   ::= 'for' IDENT 'in' Expr ':' Bloco

A produção `Alvo` restringe os alvos de atribuição a um nome, um
acesso indexado ou um acesso a campo, conforme a seção 7.2 da
especificação. Chamadas de função e literais não são alvos válidos.

## Expressões

A estratificação em níveis codifica a precedência e a associatividade
dos operadores diretamente na forma das produções, tornando a
gramática não ambígua sem necessidade de declarações externas.

    Expr         ::= ExprOu [ 'if' ExprOu 'else' Expr ]

    ExprOu       ::= ExprE { 'or' ExprE }

    ExprE        ::= ExprNao { 'and' ExprNao }

    ExprNao      ::= 'not' ExprNao
                   | ExprRel

    ExprRel      ::= ExprAdit [ OpRel ExprAdit ]

    OpRel        ::= '==' | '!=' | '<' | '<=' | '>' | '>='

    ExprAdit     ::= ExprMult { ( '+' | '-' ) ExprMult }

    ExprMult     ::= ExprPot { ( '*' | '/' | '//' | '%' ) ExprPot }

    ExprPot      ::= ExprUnaria [ '**' ExprPot ]

    ExprUnaria   ::= '-' ExprUnaria
                   | ExprPosfixa

    ExprPosfixa  ::= Atomo { Posfixo }

    Posfixo      ::= '(' [ Args ] ')'
                   | '[' Expr ']'
                   | '.' IDENT

    Args         ::= Expr { ',' Expr }

    Atomo        ::= INT_LIT
                   | FLOAT_LIT
                   | STRING_LIT
                   | BOOL_LIT
                   | 'None'
                   | 'self'
                   | IDENT
                   | 'int' | 'float' | 'str' | 'bool'
                   | '[' [ Args ] ']'
                   | 'lambda' [ IDENT { ',' IDENT } ] ':' Expr
                   | '(' Expr ')'

Em posição de expressão, `int`, `float`, `str` e `bool` são as funções
primitivas de conversão (seção 8 da especificação), e não tipos.

### Precedência resultante

Da menor para a maior:

| Nível | Operadores | Associatividade |
|---|---|---|
| 1 | `if ... else` (expressão condicional) | direita |
| 2 | `or` | esquerda |
| 3 | `and` | esquerda |
| 4 | `not` | prefixo |
| 5 | `==` `!=` `<` `<=` `>` `>=` | não associativo |
| 6 | `+` `-` | esquerda |
| 7 | `*` `/` `//` `%` | esquerda |
| 8 | `**` | direita |
| 9 | `-` unário | prefixo |
| 10 | `f(...)` `e[i]` `e.campo` | esquerda |

### Observações sobre as escolhas

**Associatividade à esquerda por repetição.** Produções da forma
`A ::= B { op B }` eliminam a recursão à esquerda, que impediria a
implementação descendente recursiva, e a árvore é construída
associando à esquerda durante o percurso da repetição.

**Potência e menos unário.** A seção 6.3 da especificação estabelece a
precedência `- unário > ** > * / // % > + -`. Os operandos de `**` são
`ExprUnaria`, de modo que `-2 ** 2` é lido como `(-2) ** 2` e `2 ** -1`
é aceito. O comportamento difere do Python 3, no qual `**` liga mais
forte que o menos unário. Na primeira versão a ordem das duas produções
estava invertida, e `-2 ** 2` saía como `-(2 ** 2)`, contra a
especificação.

**Relacionais não associativos.** A produção `ExprRel` admite no
máximo um operador relacional, portanto `a < b < c` é erro de sintaxe.
O encadeamento de comparações do Python não faz parte do MiniPython.

**Potência associa à direita.** A produção `ExprPot` é recursiva à
direita no segundo operando, de modo que `2 ** 3 ** 2` é lido como
`2 ** (3 ** 2)`.

**Lambda como átomo.** A seção 3 do enunciado determina que expressões
lambda são expressões primárias. O corpo é uma única `Expr`, o que
permite `lambda x: x if x > 0 else -x` sem ambiguidade, já que a
expressão condicional é consumida inteira como corpo.

**Literal de lista e indexação.** O colchete inicia um literal de
lista quando aparece em posição de átomo, e um acesso indexado quando
aparece em posição posfixa. A distinção é posicional e não gera
conflito.

**Blocos em linha.** A forma `if x: return 1`, com o comando na mesma
linha do cabeçalho, não é aceita. Todo bloco é delimitado por INDENT e
DEDENT, conforme a seção 3 do enunciado.

## Gramática PEG

A gramática PEG está em `src/MiniPython/Frontend/Parser/PEG.hs`,
escrita com o quasi-quoter `pegGrammar` da biblioteca typed-peg
(versão 0.6). Ela reconhece a mesma linguagem da GLC acima e produz a
mesma AST, posições incluídas. A diferença de organização é que a
typed-peg lê caracteres, e não tokens: a gramática descreve também a
parte léxica e a indentação, que na GLC chegam prontas do analisador
léxico como NEWLINE, INDENT e DEDENT.

Notação: `/` é a escolha ordenada, `&` e `!` são predicados, `*`, `+`
e `?` são repetição e opcionalidade. As anotações de layout são as da
typed-peg: `|e|` exige que o primeiro caractere de `e` esteja alinhado
com o bloco, `e^>` abre um bloco mais à direita que o comando que o
contém e `e_~` libera a coluna dentro de `e`.

Para não poluir a leitura, duas coisas ficam implícitas abaixo: todo
token consome o `Espaco` que vem depois dele, e toda palavra reservada
é seguida de `![a-zA-Z0-9_]`, para que `iffy` não seja lido como `if`
seguido de `fy`.

    Programa     <- Brancos (Ind |Decl|)* Fim

    Decl         <- DefFuncao / DefClasse / Comando

    DefFuncao    <- 'def' Nome '(' (Params ')')_~ ('->' Tipo)? ':' Suite
    Params       <- Param (',' Param)* / ε
    Param        <- 'self' / Nome (':' Tipo)?

    DefClasse    <- 'class' Nome ('(' Nome ')')? ':' FimLinha Ind Membros^>
    Membros      <- |Campo| (Ind |Campo|)* (Ind |DefFuncao|)* PassFinal?
                  / |DefFuncao| (Ind |DefFuncao|)* PassFinal?
                  / |'pass'| FimLinha
    PassFinal    <- Ind |'pass'| FimLinha
    Campo        <- Nome ':' Tipo FimLinha

    Suite        <- FimLinha Ind Bloco
    Bloco        <- (|Comando| (Ind |Comando|)*)^>

    Comando      <- ComandoIf / ComandoWhile / ComandoFor
                  / ComandoSimples FimLinha

    ComandoIf    <- 'if' Expr ':' Suite
                    (Ind |'elif'| Expr ':' Suite)*
                    (Ind |'else'| ':' Suite)?
    ComandoWhile <- 'while' Expr ':' Suite
    ComandoFor   <- 'for' Nome 'in' Expr ':' Suite

    ComandoSimples
                 <- 'pass' / 'break' / 'continue' / 'return' Expr?
                  / Nome ':' Tipo '=' !'=' Expr
                  / Expr (OpAtrib Expr)?
    OpAtrib      <- '=' !'=' / '+=' / '-=' / '*=' / '//=' / '/=' / '%='

    Tipo         <- 'int' / 'float' / 'bool' / 'str' / 'None'
                  / '(' ((Tipo (',' Tipo)*)? ')')_~ '->' Tipo
                  / Nome ('[' (Tipo ']')_~)?

    Expr         <- ExprOu ('if' ExprOu 'else' Expr)?
    ExprOu       <- ExprE ('or' ExprE)*
    ExprE        <- ExprNao ('and' ExprNao)*
    ExprNao      <- 'not' ExprNao / ExprRel
    ExprRel      <- ExprAdit (OpRel ExprAdit)?
    OpRel        <- '==' / '!=' / '<=' / '>=' / '<' / '>'
    ExprAdit     <- ExprMult (OpAdit ExprMult)*
    OpAdit       <- '+' !'=' / '-' ![=>]
    ExprMult     <- ExprPot (OpMult ExprPot)*
    OpMult       <- '//' !'=' / '/' !'=' / '%' !'=' / '*' ![*=]
    ExprPot      <- ExprUnaria ('**' ExprPot)?
    ExprUnaria   <- '-' ![=>] ExprUnaria / ExprPosfixa
    ExprPosfixa  <- Atomo Posfixo*
    Posfixo      <- '(' (Args ')')_~ / '[' (Expr ']')_~ / '.' Nome
    Args         <- Expr (',' Expr)* / ε

    Atomo        <- 'True' / 'False' / 'None' / 'self'
                  / 'int' / 'float' / 'str' / 'bool'
                  / Nome / Numero / String
                  / '[' (Args ']')_~
                  / 'lambda' (Nome (',' Nome)*)? ':' Expr
                  / '(' (Expr ')')_~

    EspacoLinha  <- ([ \t\r] / '#' [^\n]*)*
    Espaco       <- ([ \t\r] / '#' [^\n]* / &('\n' .) '\n')*
    FimLinha     <- (EspacoLinha ('\n' / !.) Brancos)_~
    Brancos      <- (EspacoLinha '\n')*_~
    Ind          <- [ \t]*_~
    Fim          <- (EspacoLinha !.)_~

    Nome         <- !Reservada [a-zA-Z_] [a-zA-Z0-9_]*
    Reservada    <- ('and' / 'break' / 'bool' / 'class' / 'continue' / 'def'
                    / 'elif' / 'else' / 'False' / 'float' / 'for' / 'if' / 'int'
                    / 'in' / 'lambda' / 'None' / 'not' / 'or' / 'pass'
                    / 'return' / 'self' / 'str' / 'True' / 'while') ![a-zA-Z0-9_]
    Numero       <- [0-9] [0-9_]* ('.' [0-9] [0-9_]*)? ([eE] [+-]? [0-9]+)?
    String       <- '"' (Escape / [^"\\\n])* '"'
                  / '\'' (Escape / [^'\\\n])* '\''
    Escape       <- '\\' [ntr\\"']

### Indentação

O exemplo `PEG.Examples.Layout` da disciplina mostra o modelo: um
bloco é `e^>`, com a coluna maior que a do comando que o abriu, e cada
comando do bloco é `|e|`, alinhado com o primeiro. O que fecha um bloco
é a primeira linha cuja coluna não alinha. Indentação inconsistente,
como voltar para uma coluna que nunca foi aberta, nem chega ao PEG: o
analisador léxico roda antes em todos os modos e rejeita o programa
com a mesma mensagem.

A relação padrão entre tokens é `gtR`: depois do primeiro token, o
restante do comando fica estritamente à direita da coluna do comando.
Por isso `elif` e `else` aparecem alinhados explicitamente, `Ind
|'elif'|`, e o programa começa na coluna 0.

### Quebras de linha

Na GLC a quebra de linha é o token NEWLINE, que o analisador léxico
não emite dentro de parênteses e colchetes. A PEG reproduz isso com o
`Espaco` entre tokens, que só consome uma quebra de linha se o primeiro
caractere da linha seguinte for aceito pela relação de coluna em vigor.
Fora de parênteses vale `gtR`, que nunca aceita a coluna 0, e a quebra
fica para `FimLinha`; dentro deles vale `_~`, que aceita qualquer
coluna, e a quebra vira espaço. A mesma regra serve aos dois casos sem
duplicar a gramática de expressões.

O mesmo cuidado vale para o fim do arquivo: sob `gtR`, `!.` no meio do
arquivo teria sucesso, porque o primeiro caractere da linha seguinte é
recusado pela relação de coluna. Por isso `Fim` e `FimLinha` testam o
fim da entrada com `_~`.

### Ordem das alternativas

A ordem é significativa em alguns pontos:

- Em `ComandoSimples`, a declaração `Nome ':' Tipo '=' Expr` vem antes
  da atribuição. Quando o `:` não aparece, a escolha volta e tenta a
  atribuição, que começa pelo mesmo `Nome`.
- Em `Atomo`, as palavras reservadas `int`, `float`, `str` e `bool`
  valem como nomes de função (seção 8 da especificação); em `Tipo`, as
  mesmas palavras são os tipos primitivos.
- Em `Reservada`, `int` precisa vir antes de `in`: a escolha é
  ordenada e não volta atrás, de modo que com `in` primeiro a entrada
  `int` falharia no `![a-zA-Z0-9_]` e seria aceita como nome.
- Nos operadores, os predicados fazem o papel do casamento mais longo
  do analisador léxico: `+` não pode ser o começo de `+=`, nem `-` o de
  `->` ou `-=`, nem `*` o de `**`.

### Posição mais à direita e mensagens

A typed-peg devolve, quando o reconhecimento falha, a posição mais à
direita em que alguma regra falhou e a lista do que era esperado ali,
como pede a seção 4 do enunciado. As diretivas `%label` dão nome às
regras que aparecem na mensagem (`expression`, `identifier`, `type`,
`NEWLINE`, `INDENT (indented block)`) e escondem, com nome vazio, o
espaço e o primeiro token das continuações opcionais (operadores,
`elif`, `else`, `->`, a vírgula de uma lista). Sem isso, em `if x > 0`
sem `:`, a lista traria todos os operadores que poderiam continuar a
expressão; com isso, traz só o `:`. O token encontrado é descrito a
partir da lista do analisador léxico, para que o texto seja igual ao
dos outros dois parsers.

As regras de comando devolvem `Either Error`, porque três restrições
só podem ser verificadas na ação semântica, que não pode fazer o
reconhecimento falhar: alvo de atribuição, parâmetro repetido e
argumento de tipo fora de `list`.

Nos testes, a mensagem do PEG é a mesma dos outros parsers em todos os
casos. Fora deles há duas diferenças conhecidas: uma palavra reservada
onde se espera um nome (`for if in xs:`) sai como `unexpected 'if'`,
porque o predicado `!Reservada` não registra o que esperava; e um
parêntese não fechado na última linha é apontado no `\n` dessa linha,
enquanto os outros apontam o NEWLINE que o analisador léxico gera no
fim do arquivo.

## Adaptação para o Happy e conflitos shift/reduce

Conforme a seção 5 do enunciado, as produções de expressão **não são
estratificadas** na especificação do Happy. Há uma única produção
`Expr` com todos os operadores, e a precedência e a associatividade
são declaradas por diretivas:

    %nonassoc LAMBDA
    %right    'if' 'else'
    %left     'or'
    %left     'and'
    %right    'not'
    %nonassoc '==' '!=' '<' '<=' '>' '>='
    %left     '+' '-'
    %left     '*' '/' '//' '%'
    %right    '**'
    %right    NEG
    %left     '.' '[' '('

A ordem das declarações é da menor para a maior precedência. O
pseudo-token `NEG` distingue o menos unário do binário, declarado com
`%prec NEG` na produção correspondente, e fica acima de `'**'`, como
na GLC. O `%nonassoc` nos operadores relacionais produz o mesmo efeito
da produção não recursiva da GLC: `a < b < c` é rejeitado.

A condicional é `%right` porque o ramo `else` da GLC é uma `Expr`
completa: `a if b else c if d else e` é `a if b else (c if d else e)`.
Na primeira versão ela estava como `%nonassoc`, e o LALR recusava essa
entrada, que os outros dois parsers aceitam.

Como o analisador léxico já insere INDENT e DEDENT, os blocos são
delimitados por tokens e a produção `Bloco` é tratada exatamente como
um par de chaves.

### Conflito encontrado

A primeira versão da especificação apresentou 16 conflitos
shift/reduce, todos concentrados em um único estado. A inspeção do
arquivo `.info` gerado pelo Happy mostrou origem única: a produção

    Expr -> 'lambda' LambdaParams ':' Expr

Ao terminar de reconhecer o corpo da lambda e encontrar um operador,
o analisador tem duas opções. Deslocar, de modo que o operador
pertença ao corpo, lendo `lambda x: x + 1` como a função que soma 1.
Ou reduzir, encerrando a lambda, lendo a mesma entrada como
`(lambda x: x) + 1`. Os 16 conflitos correspondem aos 16 operadores
que podem ocorrer nessa posição.

A resolução correta é deslocar, porque a seção 6.11 da especificação
estabelece que o corpo de uma lambda é uma expressão única.

### Resolução

Declarar `'lambda'` com baixa precedência não surtiu efeito, porque
no Happy a precedência de uma produção é herdada do seu **último**
terminal, que nessa regra é o `:`. A solução foi introduzir o
pseudo-token `LAMBDA`, declarado com a menor precedência da tabela, e
sobrescrever a precedência da produção:

    | 'lambda' LambdaParams ':' Expr %prec LAMBDA

Com isso qualquer operador tem precedência maior que a produção e o
deslocamento ocorre. A especificação passou a ser gerada com **zero
conflitos shift/reduce**.

### Ajustes para concordar com os outros parsers

Duas produções foram reorganizadas para que as verificações aconteçam
no mesmo ponto do descendente recursivo:

    DefFuncao   : 'def' ident ListaParams Retorno ':' Bloco
    ListaParams : '(' Params ')'

`ListaParams` é reduzida logo depois do `)`, antes do corpo, e é na
sua ação que o parâmetro repetido é recusado, com a posição do `)`. O
corpo de classe ganhou o `pass` opcional (`Passe`), que o descendente
recursivo já aceitava.

Para o erro sintático o Happy foi configurado com `%errorhandlertype
explist`, que entrega ao tratador os terminais aceitos no estado do
erro. Um estado LALR mistura os contextos que compartilham o mesmo
núcleo, então a lista costuma ser grande; a mensagem usa o terminal
mais informativo, nesta ordem: `:`, `INDENT`, início de expressão,
`NEWLINE` e, por fim, o primeiro terminal que não seja operador.

## Regras verificadas fora da gramática

Algumas restrições não são livres de contexto, ou ficam mais claras
como verificação à parte. Os três parsers produzem a mesma AST e
aplicam as mesmas regras, com as mesmas mensagens:

| Regra | Onde | Mensagem |
|---|---|---|
| `return` fora de função | `Syntax/Validate.hs`, sobre a AST | `'return' outside a function` |
| `break` e `continue` fora de laço | `Syntax/Validate.hs`, sobre a AST | `'break' outside a loop` |
| parâmetro repetido | em cada parser, ao fechar a lista | `duplicate parameter: 'a'` |
| alvo de atribuição | em cada parser, na atribuição | `invalid assignment target` |
| argumento de tipo fora de `list` | em cada parser, no tipo | `unexpected '[' after type 'Foo'` |

O parâmetro repetido é apontado no `)` que fecha a lista, como no
arquivo `.err` correspondente, e essa posição não fica na AST; por
isso a verificação é feita por cada parser, e não em `Validate`.

## Mensagens de erro

Todas as mensagens seguem a seção 2.1 do enunciado,
`arquivo:linha:coluna: error: mensagem`, em stderr, com o nome do
arquivo sem o diretório, como nos arquivos `.err`. Os erros léxicos
usam o texto dos `.err` (`caractere inválido`, `string não terminada`,
`literal float mal formado`, `nível de indentação inválido`); os
sintáticos têm a forma `expected X, got Y`, em que Y é o token
encontrado: `NEWLINE`, `INDENT`, `DEDENT` e `EOF` pelo nome, os demais
pelo texto entre aspas simples.

O erro de indentação é apontado no NEWLINE da linha anterior, e não no
primeiro token da linha mal indentada, porque é ao fim daquela linha
que o analisador léxico compara os níveis. É a mesma posição dos
DEDENT na saída de `--lex`.
