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
    Param        ::= IDENT [ ':' Tipo ]

    DefClasse    ::= 'class' IDENT [ '(' IDENT ')' ] ':' BlocoClasse

    BlocoClasse  ::= NEWLINE INDENT { Campo } { DefFuncao } DEDENT

    Campo        ::= IDENT ':' Tipo NEWLINE

    Bloco        ::= NEWLINE INDENT Comando { Comando } DEDENT

Os campos de uma classe precedem os métodos, conforme a seção 6.3 da
especificação. Uma classe sem campos e sem métodos tem como corpo o
comando `pass`, reconhecido pela produção `Comando`.

## Tipos

    Tipo         ::= TipoFuncao | TipoAtomico

    TipoFuncao   ::= '(' [ Tipo { ',' Tipo } ] ')' '->' Tipo

    TipoAtomico  ::= 'int' | 'float' | 'bool' | 'str' | 'None'
                   | 'list' '[' Tipo ']'
                   | IDENT

O IDENT final é resolvido pelo analisador sintático: identificadores
iniciados por letra maiúscula são tipos de classe; os demais são
variáveis de tipo (seção 3.5 da especificação).

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

    ExprMult     ::= ExprUnaria { ( '*' | '/' | '//' | '%' ) ExprUnaria }

    ExprUnaria   ::= '-' ExprUnaria
                   | ExprPot

    ExprPot      ::= ExprPosfixa [ '**' ExprPot ]

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
                   | '[' [ Args ] ']'
                   | 'lambda' [ IDENT { ',' IDENT } ] ':' Expr
                   | '(' Expr ')'

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
| 8 | `-` unário | prefixo |
| 9 | `**` | direita |
| 10 | `f(...)` `e[i]` `e.campo` | esquerda |

### Observações sobre as escolhas

**Associatividade à esquerda por repetição.** Produções da forma
`A ::= B { op B }` eliminam a recursão à esquerda, que impediria a
implementação descendente recursiva, e a árvore é construída
associando à esquerda durante o percurso da repetição.

**Potência e menos unário.** A seção 6.3 da especificação estabelece a
precedência `- unário > ** > * / // % > + -`. A gramática segue essa
ordem, de modo que `-2 ** 2` é lido como `(-2) ** 2`. O comportamento
difere do Python 3, no qual `**` liga mais forte que o menos unário.

**Relacionais não associativos.** A produção `ExprRel` admite no
máximo um operador relacional, portanto `a < b < c` é erro de sintaxe.
O encadeamento de comparações do Python não faz parte do MiniPython.

**Potência associa à direita.** A produção `ExprPot` é recursiva à
direita no segundo operando, de modo que `2 ** 3 ** 2` é lido como
`2 ** (3 ** 2)`.

**Lambda como átomo.** A seção 4 do enunciado determina que expressões
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

A gramática PEG reconhece exatamente a mesma linguagem da GLC acima
e produz a mesma AST. A notação segue a convenção usual de PEG:
`/` denota escolha ordenada, `&` e `!` denotam predicados sintáticos,
`*` e `?` denotam repetição e opcionalidade.

A diferença essencial em relação à GLC é a **escolha ordenada**: em
`A / B`, a alternativa B só é tentada quando A falha. A ordem das
alternativas passa a ter significado, e a gramática é não ambígua por
construção, sem necessidade de estratificação adicional. A
estratificação é mantida mesmo assim, porque é ela que codifica a
precedência dos operadores.

    Programa     <- Decl* EOF

    Decl         <- DefFuncao / DefClasse / Comando

    DefFuncao    <- 'def' IDENT '(' (Param (',' Param)*)? ')'
                    ('->' Tipo)? ':' Bloco

    Param        <- 'self' / IDENT (':' Tipo)?

    DefClasse    <- 'class' IDENT ('(' IDENT ')')? ':'
                    NEWLINE INDENT &Campo* DefFuncao* ('pass' NEWLINE)? DEDENT

    Campo        <- IDENT ':' Tipo NEWLINE

    Bloco        <- NEWLINE INDENT Comando+ DEDENT

    Comando      <- ComandoIf / ComandoWhile / ComandoFor
                  / ComandoSimples NEWLINE

    ComandoIf    <- 'if' Expr ':' Bloco
                    ('elif' Expr ':' Bloco)*
                    ('else' ':' Bloco)?

    ComandoWhile <- 'while' Expr ':' Bloco

    ComandoFor   <- 'for' IDENT 'in' Expr ':' Bloco

    ComandoSimples
                 <- 'pass' / 'break' / 'continue'
                  / 'return' Expr?
                  / &(IDENT ':') DeclVar
                  / Atribuicao

    DeclVar      <- IDENT ':' Tipo '=' Expr

    Atribuicao   <- Expr (OpAtrib Expr)?

    OpAtrib      <- '=' / '+=' / '-=' / '*=' / '/=' / '//=' / '%='

    Tipo         <- 'int' / 'float' / 'bool' / 'str' / 'None'
                  / '(' (Tipo (',' Tipo)*)? ')' '->' Tipo
                  / TipoNome

    TipoNome     <- IDENT ('[' Tipo ']')?

    Expr         <- ExprOu ('if' ExprOu 'else' Expr)?
    ExprOu       <- ExprE ('or' ExprE)*
    ExprE        <- ExprNao ('and' ExprNao)*
    ExprNao      <- 'not' ExprNao / ExprRel
    ExprRel      <- ExprAdit (OpRel ExprAdit)?
    ExprAdit     <- ExprMult (('+' / '-') ExprMult)*
    ExprMult     <- ExprUnaria (('*' / '/' / '//' / '%') ExprUnaria)*
    ExprUnaria   <- '-' ExprUnaria / ExprPot
    ExprPot      <- ExprPosfixa ('**' ExprPot)?
    ExprPosfixa  <- Atomo Posfixo*

    Posfixo      <- '(' (Expr (',' Expr)*)? ')'
                  / '[' Expr ']'
                  / '.' IDENT

    Atomo        <- INT_LIT / FLOAT_LIT / STRING_LIT / BOOL_LIT
                  / 'None' / 'self'
                  / IDENT / 'int' / 'float' / 'str' / 'bool'
                  / '[' (Expr (',' Expr)*)? ']'
                  / 'lambda' (IDENT (',' IDENT)*)? ':' Expr
                  / '(' Expr ')'

### Pontos de retrocesso

A implementação usa Megaparsec, no qual uma alternativa que já
consumiu tokens não permite que a seguinte seja tentada, a menos que
seja envolvida em `try`. Dois pontos exigem retrocesso:

**Declaração de variável versus atribuição.** `DeclVar` e
`Atribuicao` compartilham o prefixo IDENT e só se distinguem no
segundo token. A alternativa `DeclVar` é tentada primeiro, com
`try`, e o retrocesso ocorre quando o token seguinte ao
identificador não é `:`.

**Campo de classe versus método.** No corpo de uma classe, `Campo`
também começa com IDENT. A repetição de campos usa `try` para que o
primeiro método não seja consumido parcialmente.

Nos demais pontos a escolha é decidida pelo primeiro token, sem
retrocesso.

### Ordem das alternativas

A ordem em `ComandoSimples` e em `Atomo` é significativa. Em
`Atomo`, as palavras reservadas `int`, `float`, `str` e `bool`
aparecem após IDENT porque são funções primitivas de conversão
(seção 8 da especificação) quando ocorrem em posição de expressão.
Em `Tipo`, as mesmas palavras aparecem antes de `TipoNome`, no papel
de tipos primitivos.

### Posição mais à direita

O enunciado exige que o analisador PEG reporte a posição mais à
direita em que o reconhecimento falhou. O Megaparsec implementa esse
comportamento por construção: entre todas as alternativas tentadas, o
erro relatado é o de maior deslocamento na entrada, por ser o que
indica onde o texto efetivamente divergiu da gramática. A função
`formatPegError` converte esse deslocamento em linha e coluna
usando as posições registradas nos próprios tokens.

## Adaptação para o Happy e conflitos shift/reduce

Conforme a seção 5 do enunciado, as produções de expressão **não são
estratificadas** na especificação do Happy. Há uma única produção
`Expr` com todos os operadores, e a precedência e a associatividade
são declaradas por diretivas:

    %nonassoc LAMBDA
    %nonassoc 'if' 'else'
    %left     'or'
    %left     'and'
    %right    'not'
    %nonassoc '==' '!=' '<' '<=' '>' '>='
    %left     '+' '-'
    %left     '*' '/' '//' '%'
    %right    NEG
    %right    '**'
    %left     '.' '[' '('

A ordem das declarações é da menor para a maior precedência. O
pseudo-token `NEG` distingue o menos unário do binário, declarado com
`%prec NEG` na produção correspondente. O `%nonassoc` nos operadores
relacionais produz o mesmo efeito da produção não recursiva da GLC:
`a < b < c` é rejeitado.

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
