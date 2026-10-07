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
