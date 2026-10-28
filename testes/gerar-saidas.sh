#!/usr/bin/env bash
# Gera o arquivo testes/saidas-esperadas.txt com a saida de cada
# programa de teste, para conferencia manual e para o relatorio.
set -u
cd "$(dirname "$0")/.."

cabal build -v0 exe:mpyc || exit 1
mpyc=$(cabal list-bin mpyc)

saida="testes/saidas-esperadas.txt"
: > "$saida"

{
  echo "Saidas esperadas dos programas de teste"
  echo "Brenda Gabrielle Alves Nascimento, 24.1.4011"
  echo "BCC328 - Trabalho Pratico 1"
  echo
  echo "Programas validos: AST impressa com drawTree, identica nos tres analisadores."
  echo
} >> "$saida"

for f in mdc fib busca selecao alta_ordem formas pilha lambdas continuacao; do
  {
    echo "===================================================================="
    echo "exemplos/$f.mpy  (--parse-rd, --parse-peg e --parse-lalr)"
    echo "===================================================================="
    "$mpyc" --parse-rd "exemplos/$f.mpy" 2>&1
  } >> "$saida"
done

{
  echo "===================================================================="
  echo "Programas invalidos: mensagem de erro de cada analisador"
  echo "(a mesma do arquivo .err ao lado do programa)"
  echo "===================================================================="
  echo
} >> "$saida"

for f in erro_char erro_string erro_indent erro_sem_colon erro_if_colon erro_alvo erro_rel; do
  {
    echo "-- exemplos/$f.mpy"
    for modo in --parse-rd --parse-peg --parse-lalr; do
      printf '   %-13s %s\n' "$modo" "$("$mpyc" $modo "exemplos/$f.mpy" 2>&1 | head -1)"
    done
    echo
  } >> "$saida"
done

echo "gerado: $saida"
