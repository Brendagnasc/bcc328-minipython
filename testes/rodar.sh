#!/usr/bin/env bash
# Suite de testes do analisador lexico e sintatico do MiniPython.
#
# Verifica que os programas validos sao aceitos pelos tres
# analisadores com a mesma AST, e que os programas invalidos sao
# rejeitados com mensagem posicionada.
#
# Uso: bash testes/rodar.sh

set -u
cd "$(dirname "$0")/.."

VALIDOS="mdc fib busca selecao alta_ordem formas pilha lambdas continuacao"
INVALIDOS="erro_char erro_string erro_indent erro_sem_colon erro_if_colon erro_alvo erro_rel"

ok=0
falhas=0
tmp=$(mktemp -d)

echo "== Programas validos: os tres analisadores devem concordar"
for f in $VALIDOS; do
  src="exemplos/$f.mpy"
  cabal run -v0 mpyc -- --parse-rd   "$src" > "$tmp/$f.rd"   2>&1
  cabal run -v0 mpyc -- --parse-peg  "$src" > "$tmp/$f.peg"  2>&1
  cabal run -v0 mpyc -- --parse-lalr "$src" > "$tmp/$f.lalr" 2>&1
  if diff -q "$tmp/$f.rd" "$tmp/$f.peg" > /dev/null \
     && diff -q "$tmp/$f.rd" "$tmp/$f.lalr" > /dev/null \
     && [ -s "$tmp/$f.rd" ]; then
    echo "  ok       $f"
    ok=$((ok + 1))
  else
    echo "  FALHOU   $f"
    falhas=$((falhas + 1))
  fi
done

echo
echo "== Programas invalidos: devem ser rejeitados pelos tres"
for f in $INVALIDOS; do
  src="exemplos/$f.mpy"
  todos_rejeitam=1
  for modo in --parse-rd --parse-peg --parse-lalr; do
    if cabal run -v0 mpyc -- $modo "$src" > /dev/null 2>&1; then
      todos_rejeitam=0
    fi
  done
  if [ $todos_rejeitam -eq 1 ]; then
    echo "  ok       $f"
    echo "           $(cabal run -v0 mpyc -- --parse-rd "$src" 2>&1 | head -1)"
    ok=$((ok + 1))
  else
    echo "  FALHOU   $f (algum analisador aceitou)"
    falhas=$((falhas + 1))
  fi
done

rm -rf "$tmp"
echo
echo "Total: $ok ok, $falhas falhas"
[ $falhas -eq 0 ]
